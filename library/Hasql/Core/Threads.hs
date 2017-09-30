module Hasql.Core.Threads where

import Hasql.Prelude
import qualified Hasql.Socket as A
import qualified Data.ByteString as B


{-|
An internal request to the dispatcher.
-}
data Request

data Notification = Notification !Int32 !ByteString !ByteString

{-|
Fork off a thread, which will be fetching chunks of data from the socket, feeding them to the handler action.
-}
startReceiving :: A.Socket -> (Either Text ByteString -> IO ()) -> IO (IO ())
startReceiving socket sendResult =
  fmap killThread $ forkIO $ fix $ \loop -> do
    resultOfReceiving <- A.receive socket (shiftL 2 12)
    case resultOfReceiving of
      Right bytes ->
        if B.null bytes
          then sendResult (Left "Connection interrupted")
          else sendResult (Right bytes) >> loop
      Left msg ->
        sendResult (Left msg)

startSending :: A.Socket -> IO ByteString -> (Text -> IO ()) -> IO (IO ())
startSending socket getNextChunk sendError =
  fmap killThread $ forkIO $ fix $ \loop -> do
    bytes <- getNextChunk
    resultOfSending <- A.send socket bytes
    case resultOfSending of
      Right () -> loop
      Left msg -> sendError msg

startDispatching :: IO (Either Text ByteString) -> (ByteString -> IO ()) -> IO (Maybe Request) -> (Either Text Notification -> IO ()) -> IO (IO ())
startDispatching receive send fetchRequest sendNotification =
  $(todo "")

startMaintainingConnection :: A.Socket -> (Either Text Notification -> IO ()) -> IO (IO (), Request -> IO ())
startMaintainingConnection socket sendNotification =
  do
    inputQueue <- newTQueueIO
    outputQueue <- newTQueueIO
    requestQueue <- newTQueueIO
    stopSending <- startSending socket (atomically (readTQueue outputQueue)) (atomically . writeTQueue inputQueue . Left)
    stopReceiving <- startReceiving socket (atomically . writeTQueue inputQueue)
    stopDispatching <- startDispatching (atomically (readTQueue inputQueue)) (atomically . writeTQueue outputQueue) (atomically (tryReadTQueue requestQueue)) sendNotification
    let
      stopMaintaining = stopDispatching >> stopSending >> stopReceiving
      sendRequest = atomically . writeTQueue requestQueue
      in return (stopMaintaining, sendRequest)
