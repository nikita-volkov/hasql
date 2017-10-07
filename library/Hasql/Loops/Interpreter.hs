module Hasql.Loops.Interpreter where

import Hasql.Prelude
import Hasql.Model
import qualified Hasql.MessageTypePredicates as G
import qualified Hasql.MessageTypeNames as H
import qualified Hasql.ParseMessageStream as A
import qualified Hasql.ParseMessage as B
import qualified Hasql.ChooseMessage as F
import qualified Hasql.Protocol.Decoding as E
import qualified Hasql.Looping as C
import qualified Hasql.Choosing as I
import qualified BinaryParser as D


data ResultProcessor =
  forall result. ResultProcessor !(A.ParseMessageStream result) !(Either B.Error result -> IO ())

data UnaffiliatedResult =
  NotificationUnaffiliatedResult !Notification |
  ErrorMessageUnaffiliatedResult !ErrorMessage |
  ProtocolErrorUnaffiliatedResult !Text

loop :: IO Message -> IO (Maybe ResultProcessor) -> (UnaffiliatedResult -> IO ()) -> IO ()
loop fetchMessage fetchResultProcessor sendUnaffiliatedResult =
  fetchingMessage tryToFetchResultProcessor
  where
    fetchingMessage handler =
      do
        Message type_ payload <- fetchMessage
        handler type_ payload
    tryToFetchResultProcessor type_ payload =
      do
        fetchResult <- fetchResultProcessor
        case fetchResult of
          Just resultProcessor ->
            interpretWithResultProcessor resultProcessor type_ payload
          Nothing ->
            interpretUnaffiliatedMessage tryToFetchResultProcessor type_ payload
    interpretWithResultProcessor (ResultProcessor (A.ParseMessageStream (C.Looping (B.ParseMessage (I.Choosing typeFn)))) sendResult) =
      parseMessageStream typeFn
      where
        parseMessageStream typeFn type_ payload =
          case typeFn type_ of
            Just (ReaderT payloadFn) ->
              trace ("Interpreting a message of type \ESC[1m" <> H.string type_ <> "\ESC[0m with a result processor") $
              case payloadFn payload of
                Left parsingError -> 
                  sendResult (Left parsingError)
                Right loopingDecision -> 
                  case loopingDecision of
                    Left result -> sendResult (Right result) >> fetchingMessage tryToFetchResultProcessor
                    Right (C.Looping (B.ParseMessage (I.Choosing typeFn))) -> fetchingMessage (parseMessageStream typeFn)
            Nothing ->
              interpretUnaffiliatedMessage
                (parseMessageStream typeFn)
                type_ payload
    interpretUnaffiliatedMessage interpretNext type_ payload =
      trace ("Interpreting a message of type \ESC[1m" <> H.string type_ <> "\ESC[0m without a result processor") $
      case unaffiliatedResultTypeFn type_ of
        Just payloadFn -> sendUnaffiliatedResult (payloadFn payload) >> fetchingMessage interpretNext
        Nothing -> fetchingMessage interpretNext
      where
        F.ChooseMessage (I.Choosing unaffiliatedResultTypeFn) =
          fmap (either ProtocolErrorUnaffiliatedResult NotificationUnaffiliatedResult) F.notification <|>
          fmap (either ProtocolErrorUnaffiliatedResult ErrorMessageUnaffiliatedResult) F.error
