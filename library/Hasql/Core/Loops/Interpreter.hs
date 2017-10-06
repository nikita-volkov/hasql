module Hasql.Core.Loops.Interpreter where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.MessageTypePredicates as G
import qualified Hasql.Core.ParseMessageStream as A
import qualified Hasql.Core.ParseMessage as B
import qualified Hasql.Core.ChooseMessage as F
import qualified Hasql.Protocol.Decoding as E
import qualified Hasql.Looping as C
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
    interpretWithResultProcessor (ResultProcessor (A.ParseMessageStream (C.Looping (B.ParseMessage (Compose (F.ChooseMessage typeFn))))) sendResult) =
      parseMessageStream typeFn
      where
        parseMessageStream typeFn type_ payload =
          case typeFn type_ of
            Just payloadFn ->
              case payloadFn payload of
                Left parsingError -> sendResult (Left parsingError)
                Right loopingDecision -> case loopingDecision of
                  Left result -> sendResult (Right result) >> fetchingMessage tryToFetchResultProcessor
                  Right (C.Looping (B.ParseMessage (Compose (F.ChooseMessage typeFn)))) -> fetchingMessage (parseMessageStream typeFn)
            Nothing ->
              interpretUnaffiliatedMessage
                (parseMessageStream typeFn)
                type_ payload
    interpretUnaffiliatedMessage interpretNext type_ payload =
      case unaffiliatedResultTypeFn type_ of
        Just payloadFn -> sendUnaffiliatedResult (payloadFn payload) >> fetchingMessage interpretNext
        Nothing -> fetchingMessage interpretNext
      where
        F.ChooseMessage unaffiliatedResultTypeFn =
          fmap (either ProtocolErrorUnaffiliatedResult NotificationUnaffiliatedResult) F.notification <|>
          fmap (either ProtocolErrorUnaffiliatedResult ErrorMessageUnaffiliatedResult) F.error
