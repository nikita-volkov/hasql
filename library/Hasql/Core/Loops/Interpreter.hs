module Hasql.Core.Loops.Interpreter where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.MessageTypePredicates as G
import qualified Hasql.Protocol.Decoding as E
import qualified BinaryParser as D


loop :: IO Message -> IO (Maybe ResultProcessor) -> (Notification -> IO ()) -> (Text -> IO ()) -> (ErrorMessage -> IO ()) -> IO ()
loop fetchMessage fetchResultProcessor sendNotification sendProtocolError sendErrorMessage =
  fetchingMessage interpretNeutrally
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
              _ -> $(todo "")
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
