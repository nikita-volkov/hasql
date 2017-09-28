module Hasql.Core.IO where

import Hasql.Prelude
import qualified Hasql.Socket as A
import qualified Data.ByteString as B


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

startInteracting :: IO (Either Text ByteString) -> (ByteString -> IO ()) -> IO (IO ())
startInteracting receive send =
  $(todo "")

startMaintainingConnection :: A.Socket -> IO (IO ())
startMaintainingConnection socket =
  do
    inputQueue <- newTQueueIO
    outputQueue <- newTQueueIO
    stopSending <- startSending socket (atomically (readTQueue outputQueue)) (atomically . writeTQueue inputQueue . Left)
    stopReceiving <- startReceiving socket (atomically . writeTQueue inputQueue)
    stopInteracting <- startInteracting (atomically (readTQueue inputQueue)) (atomically . writeTQueue outputQueue)
    return (stopInteracting >> stopSending >> stopReceiving)
