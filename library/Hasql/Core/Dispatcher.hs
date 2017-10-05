module Hasql.Core.Dispatcher where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Socket as A
import qualified ByteString.StrictBuilder as B
import qualified BinaryParser as D
import qualified PtrMagic.Encoding as F
import qualified Hasql.Core.ParseMessageStream as E
import qualified Hasql.Core.Request as C
import qualified Hasql.Core.Loops.Serializer as SerializerLoop
import qualified Hasql.Core.Loops.Receiver as ReceiverLoop
import qualified Hasql.Core.Loops.Sender as SenderLoop
import qualified Hasql.Core.Loops.IncomingMessagesSlicer as IncomingMessagesSlicerLoop
import qualified Hasql.Core.Loops.Interpreter as InterpreterLoop


data Dispatcher =
  Dispatcher {
    stop :: IO (),
    performRequest :: forall result. C.Request result -> IO (Either Error result)
  }

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
      performRequest :: forall result. C.Request result -> IO (Either Error result)
      performRequest (C.Request builder parse) =
        do
          resultVar <- newEmptyTMVarIO
          atomically $ do
            writeTQueue serializerMessageQueue (SerializerLoop.SerializeMessage encoding)
            writeTQueue resultProcessorQueue (InterpreterLoop.ResultProcessor parseWithError (sendResult resultVar))
          atomically (takeTMVar resultVar)
        where
          encoding =
            B.builderPtrFiller builder F.Encoding
          parseWithError =
            fmap (either (Left . ProtocolError) Right) (runExceptT parse) <|>
            fmap (Left . either ProtocolError backendError) E.error
            where
              backendError (ErrorMessage state details) = BackendError state details
          sendResult resultVar resultWithError =
            do
              result <- atomically (fmap (Left . TransportError) (readTMVar transportErrorVar) <|> pure resultWithError)
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


