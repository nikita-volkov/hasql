module Hasql.Dispatcher where

import Hasql.Prelude
import Hasql.Model
import qualified Hasql.Socket as A
import qualified ByteString.StrictBuilder as B
import qualified BinaryParser as D
import qualified PtrMagic.Encoding as F
import qualified Hasql.ParseMessageStream as E
import qualified Hasql.ParseMessage as H
import qualified Hasql.Request as C
import qualified Hasql.InteractUnauthenticated as G
import qualified Hasql.Loops.Serializer as SerializerLoop
import qualified Hasql.Loops.Receiver as ReceiverLoop
import qualified Hasql.Loops.Sender as SenderLoop
import qualified Hasql.Loops.IncomingMessagesSlicer as IncomingMessagesSlicerLoop
import qualified Hasql.Loops.Interpreter as InterpreterLoop


data Dispatcher =
  Dispatcher {
    stop :: IO (),
    performRequest :: PerformRequest
  }

type PerformRequest =
  forall result. C.Request result -> IO (Either Error result)

start :: A.Socket -> (Either Error Notification -> IO ()) -> IO Dispatcher
start socket sendErrorOrNotification =
  do
    outgoingBytesQueue <- newTQueueIO
    incomingBytesQueue <- newTQueueIO
    serializerMessageQueue <- newTQueueIO
    incomingMessageQueue <- newTQueueIO
    resultProcessorQueue <- newTQueueIO
    transportErrorVar <- newEmptyTMVarIO
    let
      performRequest :: PerformRequest
      performRequest (C.Request builder parseExcept) =
        do
          resultVar <- newEmptyTMVarIO
          atomically $ do
            writeTQueue resultProcessorQueue (InterpreterLoop.ResultProcessor parse (sendResult resultVar))
            writeTQueue serializerMessageQueue (SerializerLoop.SerializeMessage encoding)
            writeTQueue serializerMessageQueue (SerializerLoop.FlushMessage)
          atomically (takeTMVar resultVar)
        where
          encoding =
            B.builderPtrFiller builder F.Encoding
          parse =
            fmap (either (\(C.BackendError state details) -> Left (BackendError state details)) Right) (runExceptT parseExcept)
          sendResult resultVar resultOrError =
            do
              result <- case resultOrError of
                Left message -> return (Left (DecodingError message))
                Right result -> atomically (fmap (Left . TransportError) (readTMVar transportErrorVar) <|> pure result)
              atomically (putTMVar resultVar result)
      loopSending =
        SenderLoop.loop socket
          (atomically (readTQueue outgoingBytesQueue))
          (atomically . putTMVar transportErrorVar)
      loopReceiving =
        ReceiverLoop.loop socket
          (atomically . writeTQueue incomingBytesQueue)
          (atomically . putTMVar transportErrorVar)
      loopSerializing =
        SerializerLoop.loop
          (atomically (readTQueue serializerMessageQueue))
          (atomically . writeTQueue outgoingBytesQueue)
      loopSlicing =
        IncomingMessagesSlicerLoop.loop
          (atomically (readTQueue incomingBytesQueue))
          (atomically . writeTQueue incomingMessageQueue)
      loopInterpreting =
        InterpreterLoop.loop
          (atomically (readTQueue incomingMessageQueue))
          (atomically (tryReadTQueue resultProcessorQueue))
          (\case
            InterpreterLoop.NotificationUnaffiliatedResult notification ->
              sendErrorOrNotification (Right notification)
            InterpreterLoop.ErrorMessageUnaffiliatedResult (ErrorMessage state details) ->
              sendErrorOrNotification (Left (BackendError state details))
            InterpreterLoop.ProtocolErrorUnaffiliatedResult details ->
              sendErrorOrNotification (Left (ProtocolError details)))
      in do
        kill <- startThreads [loopInterpreting, loopSerializing, loopSlicing, loopSending, loopReceiving]
        return (Dispatcher kill performRequest)

interact :: Dispatcher -> G.Interact result -> IO (Either Error result)
interact dispatcher (G.Interact free) =
  runExceptT $ iterM interpretFreeRequest free
  where
    interpretFreeRequest request =
      join (ExceptT (performRequest dispatcher request))
