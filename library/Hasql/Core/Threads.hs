module Hasql.Core.Threads where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Socket as A
import qualified Hasql.Core.StreamReader as C
import qualified Hasql.Core.MessageTypePredicates as G
import qualified Hasql.Protocol.Decoding as E
import qualified Data.ByteString as B
import qualified BinaryParser as D


{-|
Fork off a thread, which will be fetching chunks of data from the socket, feeding them to the handler action.
-}
startReceiving :: A.Socket -> (ByteString -> IO ()) -> (Text -> IO ()) -> IO (IO ())
startReceiving socket sendResult reportError =
  fmap killThread $ forkIO $ fix $ \loop -> do
    resultOfReceiving <- A.receive socket (shiftL 2 12)
    case resultOfReceiving of
      Right bytes ->
        if B.null bytes
          then reportError "Connection interrupted"
          else sendResult bytes >> loop
      Left msg ->
        reportError msg

startSending :: A.Socket -> IO ByteString -> (Text -> IO ()) -> IO (IO ())
startSending socket getNextChunk reportError =
  fmap killThread $ forkIO $ fix $ \loop -> do
    bytes <- getNextChunk
    resultOfSending <- A.send socket bytes
    case resultOfSending of
      Right () -> loop
      Left msg -> reportError msg

startSlicing :: IO ByteString -> (Message -> IO ()) -> IO (IO ())
startSlicing getNextChunk sendMessage =
  fmap killThread (forkIO (C.run read getNextChunk $> ()))
  where
    read =
      do
        message <- C.fetchMessage Message
        liftIO (sendMessage message)
        read

startInterpreting :: IO Message -> IO (Maybe ResultProcessor) -> (Notification -> IO ()) -> (Text -> IO ()) -> (ErrorMessage -> IO ()) -> IO (IO ())
startInterpreting fetchMessage fetchResultProcessor sendNotification sendProtocolError sendErrorMessage =
  fmap killThread (forkIO (fetchingMessage interpretNeutrally))       
  where
    fetchingMessage handler =
      do
        Message type_ payload <- fetchMessage
        handler type_ payload
    interpretNeutrally type_ payload =
      if
        | G.dataRow type_ || G.commandComplete type_ || G.emptyQuery type_ -> do
          fetchResult <- fetchResultProcessor
          case fetchResult of
            Nothing -> fetchingMessage interpretNeutrally
            Just resultProcessor -> case resultProcessor of
              RowsResultProcessor sendRow sendEnd ->
                interpretCollectingRows sendRow sendEnd type_ payload
              RowsAffectedResultProcessor sendAmount ->
                interpretAffectedRows sendAmount type_ payload
        | True -> interpretNonResult interpretNeutrally type_ payload
    interpretCollectingRows sendRow sendEnd type_ payload =
      if
        | G.dataRow type_ -> sendRow payload >> fetchingMessage (interpretCollectingRows sendRow sendEnd)
        | G.commandComplete type_ -> sendEnd >> fetchingMessage interpretNeutrally
        | G.emptyQuery type_ -> sendEnd >> fetchingMessage interpretNeutrally
        | True -> interpretNonResult (interpretCollectingRows sendRow sendEnd) type_ payload
    interpretAffectedRows sendAmount type_ payload =
      if
        | G.commandComplete type_ -> case D.run E.commandCompleteMessageAffectedRows payload of
          Right amount -> do
            sendAmount amount
            fetchingMessage interpretNeutrally
          Left parsingError -> do
            sendProtocolError ("CommandComplete parsing error: " <> parsingError)
            fetchingMessage interpretNeutrally
        | G.dataRow type_ -> fetchingMessage (interpretAffectedRows sendAmount)
        | G.emptyQuery type_ -> sendAmount 0 >> fetchingMessage interpretNeutrally
        | True -> interpretNonResult (interpretAffectedRows sendAmount) type_ payload
    interpretNonResult interpretNext type_ payload =
      if
        | G.notification type_ -> case D.run (E.notificationMessage Notification) payload of
          Right notification -> do
            sendNotification notification
            fetchingMessage interpretNext
          Left parsingError -> do
            sendProtocolError ("Notification parsing error: " <> parsingError)
            fetchingMessage interpretNext
        | G.error type_ -> case D.run (E.errorMessage ErrorMessage) payload of
          Right errorMessage -> do
            sendErrorMessage errorMessage
            fetchingMessage interpretNext
          Left parsingError -> do
            sendProtocolError ("ErrorResponse parsing error: " <> parsingError)
            fetchingMessage interpretNext
        | True -> fetchingMessage interpretNext

startDispatching :: A.Socket -> IO (IO ())
startDispatching socket =
  do
    inputQueue <- newTQueueIO
    outputQueue <- newTQueueIO
    messageQueue <- newTQueueIO
    errorVar <- newEmptyTMVarIO
    stopSending <- startSending socket (atomically (readTQueue outputQueue)) (atomically . putTMVar errorVar . TransportError)
    stopReceiving <- startReceiving socket (atomically . writeTQueue inputQueue) (atomically . putTMVar errorVar . TransportError)
    stopSlicing <- startSlicing (atomically (readTQueue inputQueue)) (atomically . writeTQueue messageQueue)
    let
      stopMaintaining = stopSlicing >> stopReceiving >> stopSending
      in return (stopMaintaining)
