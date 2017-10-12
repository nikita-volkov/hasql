module Hasql.Core.Dispatcher where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.Socket as A
import qualified ByteString.StrictBuilder as B
import qualified BinaryParser as D
import qualified Hasql.Core.Request as C
import qualified Hasql.Core.InteractUnauthenticated as G
import qualified Hasql.Core.Loops.Serializer as H
import qualified Hasql.Core.Loops.Receiver as I
import qualified Hasql.Core.Loops.Sender as J
import qualified Hasql.Core.Loops.Interpreter as K


data Dispatcher =
  Dispatcher
    !ThreadId !ThreadId !ThreadId !ThreadId
    !(TQueue ByteString) !(TQueue H.Message) !(TQueue Response) !(TQueue K.ResultProcessor) !(TMVar Text)

start :: A.Socket -> (Notification -> IO ()) -> IO Dispatcher
start socket sendNotification =
  do
    outgoingBytesQueue <- newTQueueIO
    serializerMessageQueue <- newTQueueIO
    responseQueue <- newTQueueIO
    resultProcessorQueue <- newTQueueIO
    transportErrorVar <- newEmptyTMVarIO
    interpreterTid <-
      forkIO (K.loop
        (atomically (readTQueue responseQueue))
        (atomically (tryReadTQueue resultProcessorQueue))
        (sendNotification))
    serializerTid <-
      forkIO (H.loop
        (atomically (readTQueue serializerMessageQueue))
        (atomically . writeTQueue outgoingBytesQueue))
    senderTid <-
      forkIO (J.loop socket
        (atomically (readTQueue outgoingBytesQueue))
        (atomically . putTMVar transportErrorVar))
    receiverTid <-
      forkIO (I.loop socket
        (atomically . writeTQueue responseQueue)
        (atomically . putTMVar transportErrorVar))
    return (Dispatcher interpreterTid serializerTid senderTid receiverTid
      outgoingBytesQueue serializerMessageQueue responseQueue resultProcessorQueue transportErrorVar)

performRequest :: Dispatcher -> C.Request result -> IO (Either Error result)
performRequest (Dispatcher _ _ _ _ _ serializerMessageQueue _ resultProcessorQueue transportErrorVar) (C.Request builder ir) =
  do
    resultVar <- newEmptyTMVarIO
    atomically $ do
      writeTQueue resultProcessorQueue (K.ResultProcessor ir (atomically . putTMVar resultVar))
      writeTQueue serializerMessageQueue (H.SerializeMessage builder)
      writeTQueue serializerMessageQueue (H.FlushMessage)
    atomically (fmap (Left . TransportError) (readTMVar transportErrorVar) <|> takeTMVar resultVar)

stop :: Dispatcher -> IO ()
stop (Dispatcher interpreterTid serializerTid senderTid receiverTid _ _ _ _ _) =
  do
    killThread interpreterTid
    killThread serializerTid
    killThread senderTid
    killThread receiverTid

interact :: Dispatcher -> G.Interact result -> IO (Either Error result)
interact dispatcher (G.Interact free) =
  runExceptT $ iterM interpretFreeRequest free
  where
    interpretFreeRequest request =
      join (ExceptT (performRequest dispatcher request))
