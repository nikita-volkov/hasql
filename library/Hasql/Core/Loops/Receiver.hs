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
          {-# SCC "receiveToBuffer" #-} 
          C.push buffer 4096 $ \ptr -> do
            result <- A.receiveToPtr socket ptr 4096
            case result of
              Right amountReceived -> return (amountReceived, succeed)
              Left error -> return (0, reportTransportError error)
        ensuringBufferHasData :: IO () -> IO ()
        ensuringBufferHasData succeed =
          {-# SCC "ensuringBufferHasData" #-} 
          do
            space <- C.getSpace buffer
            if space == 0
              then join (receiveToBuffer succeed)
              else succeed
        peekFromBuffer :: D.Peek (IO ()) -> IO ()
        peekFromBuffer (D.Peek amount ptrIO) =
          {-# SCC "peekFromBuffer" #-} 
          fix $ \ recur ->
          join $ C.pull buffer amount ptrIO $ \ _ ->
          receiveToBuffer recur
        parseNextResponse :: IO () -> J.ParseResponse (IO ()) -> IO ()
        parseNextResponse ignore (J.ParseResponse parseResponseChurch) =
          {-# SCC "parseNextResponse" #-} 
          peekFromBuffer peek
          where
            peek =
              E.messageTypeAndLength $ \ !type_ !length ->
              parseResponseChurch type_
                (peekFromBuffer (D.Peek length (const (pure ignore))))
                (\ parse -> peekFromBuffer (D.parse length
                  parse
                  (const (reportProtocolError "Parser consumed more data than it was supposed to"))
                  reportProtocolError))
        parseNextResponseSequence :: IO ()
        parseNextResponseSequence =
          {-# SCC "parseNextResponseSequence" #-} 
          ensuringBufferHasData $
          fetchResultProcessor >>= \case
            Just resultProcessor ->
              parseNextResponseSequenceWithResultProcessor resultProcessor
            Nothing ->
              parseNextResponse parseNextResponseSequence parseResponse
              where
                parseResponse =
                  fmap (\ send -> send >> parseNextResponseSequence) (J.notification sendNotification)
        parseNextResponseSequenceWithResultProcessor :: ResultProcessor -> IO ()
        parseNextResponseSequenceWithResultProcessor (ResultProcessor (B.ParseResponses (ExceptT (F run))) reportParsingError reportBackendError) =
          {-# SCC "parseNextResponseSequenceWithResultProcessor" #-}
          run pureCase liftCase
          where
            pureCase :: Either Text (IO ()) -> IO ()
            pureCase result =
              case result of
                Right send -> send >> parseNextResponseSequence
                Left error -> reportParsingError error
            liftCase :: J.ParseResponse (IO ()) -> IO ()
            liftCase rpParseResponse =
              loop
              where
                loop =
                  parseNextResponse loop parseResponse
                parseResponse =
                  rpParseResponse <|>
                  fmap (\ send -> send >> loop) (J.notification sendNotification) <|>
                  fmap (\ report -> report >> parseNextResponseSequence) (J.error reportBackendError)
