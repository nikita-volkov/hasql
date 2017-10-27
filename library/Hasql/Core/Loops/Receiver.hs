module Hasql.Core.Loops.Receiver where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.Socket as A
import qualified Hasql.Core.Protocol.Peek as E
import qualified Hasql.Core.Protocol.Parse.Responses as F
import qualified Hasql.Core.MessageTypeNames as H
import qualified Hasql.Core.MessageTypePredicates as G
import qualified Hasql.Core.ParseResponses as B
import qualified Buffer as C
import qualified Ptr.Peek as D
import qualified Ptr.Parse as I


data ResultProcessor =
  ResultProcessor
    (B.ParseResponses (IO ()))
    (Text -> IO ())
    (ByteString -> ByteString -> IO ())


loop :: A.Socket -> IO (Maybe ResultProcessor) -> (Word32 -> ByteString -> ByteString -> IO ()) -> (Text -> IO ()) -> (Text -> IO ()) -> IO ()
loop socket fetchResultProcessor sendNotification reportTransportError reportProtocolError =
  {-# SCC "loop" #-} 
  do
    buffer <- C.new (shiftL 2 15)
    withBuffer buffer
  where
    withBuffer buffer =
      parseNextResponse
      where
        receiveToBuffer failure success =
          C.push buffer 4096 $ \ptr -> do
            result <- A.receiveToPtr socket ptr 4096
            case result of
              Right amountReceived -> return (amountReceived, success)
              Left error -> return (0, failure error)

        peekFromBuffer :: D.Peek a -> (a -> IO ()) -> IO ()
        peekFromBuffer (D.Peek amount ptrIO) succeed =
          fix $ \ recur ->
          join $ C.pull buffer amount (fmap succeed . {-# SCC "loop/peeking" #-} ptrIO) $ \ _ ->
          receiveToBuffer reportTransportError recur

        parseNextResponseBody :: (Word8 -> IO (I.Parse (IO ()))) -> IO ()
        parseNextResponseBody cont =
          peekFromBuffer peek $ \ bodyPeekIO -> do
            bodyPeek <- bodyPeekIO
            peekFromBuffer bodyPeek id
          where
            peek =
              E.messageTypeAndLength $ \ !type_ !length ->
              do
                parse <- cont type_
                return (D.parse length parse 
                  (const (reportProtocolError "Parser consumed more data than it was supposed to"))
                  reportProtocolError)

        parseNextResponse :: IO ()
        parseNextResponse =
          parseNextResponseBody $ \ type_ ->
          flip fmap fetchResultProcessor $ \case
            Just resultProcessor ->
              parseBodyWithResultProcessor resultProcessor type_
            Nothing ->
              if
                | G.notification type_ ->
                  flip fmap (F.notificationBody sendNotification) $ \ send ->
                  send >> parseNextResponse
                | otherwise ->
                  pure parseNextResponse
          where
            parseBodyWithResultProcessor :: ResultProcessor -> Word8 -> I.Parse (IO ())
            parseBodyWithResultProcessor (ResultProcessor parseResponses reportParsingError reportBackendError) =
              parseBodyWithParseResponses parseResponses
              where
                parseBodyWithParseResponses (B.ParseResponses parse) type_ =
                  parse type_ reportProtocolError parseNextResponseWithParseResponses sendResultAndLoop altParse
                  where
                    parseNextResponseWithParseResponses parseResponses =
                      parseNextResponseBody $ \ type_ ->
                      return (parseBodyWithParseResponses parseResponses type_)
                    sendResultAndLoop sendResult =
                      sendResult >> parseNextResponse
                    altParse =
                      if
                        | G.notification type_ ->
                          flip fmap (F.notificationBody sendNotification) $ \ send ->
                          send >> parseNextResponseWithParseResponses (B.ParseResponses parse)
                        | G.error type_ ->
                          flip fmap (F.errorResponseBody reportBackendError) $ \ report ->
                          report >> parseNextResponse
                        | otherwise ->
                          pure (parseNextResponseWithParseResponses (B.ParseResponses parse))
