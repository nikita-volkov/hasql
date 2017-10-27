module Hasql.Core.Dispatcher where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.Socket as A
import qualified Hasql.Core.Request as C
import qualified Hasql.Core.UnauthenticatedSession as G
import qualified Hasql.Core.Loops.Serializer as H
import qualified Hasql.Core.Loops.Receiver as I
import qualified Hasql.Core.Loops.Sender as J


data Dispatcher =
  Dispatcher
    !ThreadId !ThreadId !ThreadId
    !(TQueue ByteString) !(TQueue H.Message) !(TQueue I.ResultProcessor) !(TMVar Text)

start :: A.Socket -> (Notification -> IO ()) -> IO Dispatcher
start socket sendNotification =
  do
    outgoingBytesQueue <- newTQueueIO
    serializerMessageQueue <- newTQueueIO
    resultProcessorQueue <- newTQueueIO
    fatalErrorVar <- newEmptyTMVarIO
    serializerTid <-
      forkIO (H.loop
        (atomically (readTQueue serializerMessageQueue))
        (atomically . writeTQueue outgoingBytesQueue))
    senderTid <-
      forkIO (J.loop socket
        (atomically (readTQueue outgoingBytesQueue))
        (atomically . putTMVar fatalErrorVar))
    receiverTid <-
      forkIO (I.loop socket
        (atomically (tryReadTQueue resultProcessorQueue))
        (\a b c -> sendNotification (Notification a b c))
        (atomically . putTMVar fatalErrorVar)
        (atomically . putTMVar fatalErrorVar))
    return (Dispatcher serializerTid senderTid receiverTid
      outgoingBytesQueue serializerMessageQueue resultProcessorQueue fatalErrorVar)

performRequest :: Dispatcher -> C.Request result -> IO (Either Error result)
performRequest (Dispatcher _ _ _ _ serializerMessageQueue resultProcessorQueue fatalErrorVar) (C.Request builder parseResponses) =
  do
    resultVar <- newEmptyTMVarIO
    atomically $ do
      writeTQueue resultProcessorQueue
        (I.ResultProcessor
          (fmap (atomically . putTMVar resultVar . Right) parseResponses)
          $(todo "")
          (\ code details -> (atomically . putTMVar resultVar . Left) (BackendError code details)))
      writeTQueue serializerMessageQueue (H.SerializeMessage builder)
      writeTQueue serializerMessageQueue (H.FlushMessage)
    atomically (fmap (Left . FatalError) (readTMVar fatalErrorVar) <|> takeTMVar resultVar)

stop :: Dispatcher -> IO ()
stop (Dispatcher serializerTid senderTid receiverTid _ _ _ _) =
  do
    killThread serializerTid
    killThread senderTid
    killThread receiverTid

interact :: Dispatcher -> G.Session result -> IO (Either Error result)
interact dispatcher (G.Session free) =
  runExceptT $ iterM interpretFreeRequest free
  where
    interpretFreeRequest request =
      join (ExceptT (performRequest dispatcher request))
