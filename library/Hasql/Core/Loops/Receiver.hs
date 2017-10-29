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
        receivingToBuffer :: IO () -> IO ()
        receivingToBuffer succeed =
          join (receiveToBuffer succeed)
        ensuringBufferHasData :: IO () -> IO ()
        ensuringBufferHasData succeed =
          do
            space <- C.getSpace buffer
            if space == 0
              then receivingToBuffer succeed
              else succeed
        peekingFromBuffer :: D.Peek (IO ()) -> IO ()
        peekingFromBuffer (D.Peek amount ptrIO) =
          fix $ \ recur ->
          join $ C.pull buffer amount ptrIO $ \ _ ->
          receiveToBuffer recur
        parsingNextResponse :: IO () -> J.ParseResponse (IO ()) -> IO ()
        parsingNextResponse ignore (J.ParseResponse parseResponseChurch) =
          {-# SCC "parsingNextResponse" #-} 
          peekingFromBuffer peek
          where
            peek =
              E.messageTypeAndLength $ \ !type_ !length ->
              trace ("parsingNextResponse: " <> H.string type_) $
              parseResponseChurch type_
                (peekingFromBuffer (D.Peek length (const (pure ignore))))
                (\ parse -> peekingFromBuffer (D.parse length
                  parse
                  (const (reportProtocolError "Parser consumed more data than it was supposed to"))
                  reportProtocolError))
        parseNextResponseSequence :: IO ()
        parseNextResponseSequence =
          trace "parseNextResponseSequence" $
          ensuringBufferHasData $
          fetchResultProcessor >>= \case
            Just resultProcessor ->
              parseNextResponseSequenceWithResultProcessor resultProcessor
            Nothing ->
              parsingNextResponse parseNextResponseSequence parseResponse
              where
                parseResponse =
                  fmap (\ send -> send >> parseNextResponseSequence) (J.notification sendNotification)
        parseNextResponseSequenceWithResultProcessor :: ResultProcessor -> IO ()
        parseNextResponseSequenceWithResultProcessor (ResultProcessor (B.ParseResponses (ExceptT free)) reportParsingError reportBackendError) =
          runFree free
          where
            runFree :: F J.ParseResponse (Either Text (IO ())) -> IO ()
            runFree (F run) =
              run pureCase liftCase
              where
                pureCase :: Either Text (IO ()) -> IO ()
                pureCase result =
                  case result of
                    Left error -> reportProtocolError error
                    Right send -> send >> parseNextResponseSequence
                liftCase :: J.ParseResponse (IO ()) -> IO ()
                liftCase rpParseResponse =
                  fix $ \ recur ->
                  parsingNextResponse recur parseResponse
                  where
                    parseResponse =
                      rpParseResponse <|> J.notification sendNotification <|> J.error reportBackendError
