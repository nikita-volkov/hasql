module Hasql.Core.Loops.Receiver where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.Socket as A
import qualified Hasql.Core.Protocol.Peek as E
import qualified Hasql.Core.Protocol.Parse.Responses as F
import qualified Hasql.Core.MessageTypeNames as H
import qualified Hasql.Core.MessageTypePredicates as G
import qualified Hasql.Core.ParseResponses as B
import qualified Hasql.Core.ParseResponse as J
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
      parseNextResponseSequence
      where
        receiveToBuffer :: IO () -> IO (IO ())
        receiveToBuffer succeed =
          C.push buffer 4096 $ \ptr -> do
            result <- A.receiveToPtr socket ptr 4096
            case result of
              Right amountReceived -> return (amountReceived, succeed)
              Left error -> return (0, reportTransportError error)
        peekFromBuffer :: D.Peek a -> (a -> IO ()) -> IO ()
        peekFromBuffer (D.Peek amount ptrIO) succeed =
          fix $ \ recur ->
          join $ C.pull buffer amount (fmap succeed . {-# SCC "loop/peeking" #-} ptrIO) $ \ _ ->
          receiveToBuffer recur
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
        parseNextResponseSequence :: IO ()
        parseNextResponseSequence =
          trace "parseNextResponseSequence" $
          parseNextResponseBody $ \type_ ->
          fetchResultProcessor >>= \case
            Just resultProcessor ->
              resultProcessorBodyParser resultProcessor type_
            Nothing ->
              if
                | G.notification type_ ->
                  return $
                  flip fmap (F.notificationBody sendNotification) $ \ send ->
                  send >> parseNextResponseSequence
                | otherwise ->
                  return (pure parseNextResponseSequence)
        resultProcessorBodyParser :: ResultProcessor -> Word8 -> IO (I.Parse (IO ()))
        resultProcessorBodyParser (ResultProcessor (B.ParseResponses (ExceptT free)) reportParsingError reportBackendError) type_ =
          freeBodyParser free type_
          where
            freeBodyParser :: F J.ParseResponse (Either Text (IO ())) -> Word8 -> IO (I.Parse (IO ()))
            freeBodyParser (F unlift) =
              unlift pureCase liftCase
              where
                pureCase :: Either Text (IO ()) -> Word8 -> IO (I.Parse (IO ()))
                pureCase result type_ =
                  trace ("pureCase: " <> H.string type_) $
                  case result of
                    Left error -> return (return (reportProtocolError error))
                    Right send -> return (return (send >> parseNextResponseSequence))
                liftCase :: J.ParseResponse (Word8 -> IO (I.Parse (IO ()))) -> Word8 -> IO (I.Parse (IO ()))
                liftCase (J.ParseResponse parseResponse) type_ =
                  parseResponse type_ yieldCase parseCase
                  where
                    yieldCase :: IO (I.Parse (IO ()))
                    yieldCase =
                      trace ("liftCase/yieldCase: " <> H.string type_) $
                      return $
                      if
                        | G.notification type_ ->
                          flip fmap (F.notificationBody sendNotification) $ \ send ->
                          send >> interpretOnNextResponse (F unlift)
                        | G.error type_ ->
                          flip fmap (F.errorResponseBody reportBackendError) $ \ report ->
                          report >> parseNextResponseSequence
                        | otherwise ->
                          pure (interpretOnNextResponse (F unlift))
                    parseCase :: I.Parse (Word8 -> IO (I.Parse (IO ()))) -> IO (I.Parse (IO ()))
                    parseCase parser =
                      trace ("liftCase/parseCase: " <> H.string type_) $
                      return (fmap parseNextResponseBody parser)
            interpretOnNextResponse :: F J.ParseResponse (Either Text (IO ())) -> IO ()
            interpretOnNextResponse f =
              parseNextResponseBody (freeBodyParser f)
