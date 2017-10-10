module Hasql.Dispatcher where

import Hasql.Prelude
import Hasql.Model
import qualified Hasql.Socket as A
import qualified ByteString.StrictBuilder as B
import qualified BinaryParser as D
import qualified PtrMagic.Encoding as F
import qualified Hasql.Request as C
import qualified Hasql.InteractUnauthenticated as G
import qualified Hasql.Loops.Serializer as SerializerLoop
import qualified Hasql.Loops.Receiver as ReceiverLoop
import qualified Hasql.Loops.Sender as SenderLoop
import qualified Hasql.Loops.Interpreter as InterpreterLoop


data Dispatcher =
  Dispatcher {
    stop :: IO (),
    performRequest :: PerformRequest
  }

type PerformRequest =
  forall result. C.Request result -> IO (Either Error result)

start :: A.Socket -> (Notification -> IO ()) -> IO Dispatcher
start socket sendNotification =
  do
    outgoingBytesQueue <- newTQueueIO
    serializerMessageQueue <- newTQueueIO
    responseQueue <- newTQueueIO
    resultProcessorQueue <- newTQueueIO
    transportErrorVar <- newEmptyTMVarIO
    let
      performRequest :: PerformRequest
      performRequest (C.Request builder ir) =
        do
          resultVar <- newEmptyTMVarIO
          atomically $ do
            writeTQueue resultProcessorQueue (InterpreterLoop.ResultProcessor ir (atomically . putTMVar resultVar))
            writeTQueue serializerMessageQueue (SerializerLoop.SerializeMessage encoding)
            writeTQueue serializerMessageQueue (SerializerLoop.FlushMessage)
          atomically (fmap (Left . TransportError) (readTMVar transportErrorVar) <|> takeTMVar resultVar)
        where
          encoding =
            B.builderPtrFiller builder F.Encoding
      loopSending =
        SenderLoop.loop socket
          (atomically (readTQueue outgoingBytesQueue))
          (atomically . putTMVar transportErrorVar)
      loopReceiving =
        ReceiverLoop.loop socket
          (atomically . writeTQueue responseQueue)
          (atomically . putTMVar transportErrorVar)
      loopSerializing =
        SerializerLoop.loop
          (atomically (readTQueue serializerMessageQueue))
          (atomically . writeTQueue outgoingBytesQueue)
      loopInterpreting =
        InterpreterLoop.loop
          (atomically (readTQueue responseQueue))
          (atomically (tryReadTQueue resultProcessorQueue))
          (sendNotification)
      in do
        kill <- startThreads [loopInterpreting, loopSerializing, loopSending, loopReceiving]
        return (Dispatcher kill performRequest)

interact :: Dispatcher -> G.Interact result -> IO (Either Error result)
interact dispatcher (G.Interact free) =
  runExceptT $ iterM interpretFreeRequest free
  where
    interpretFreeRequest request =
      join (ExceptT (performRequest dispatcher request))
