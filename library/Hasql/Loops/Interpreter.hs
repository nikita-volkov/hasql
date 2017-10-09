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
import qualified Control.Monad.Free.Church as J


data ResultProcessor =
  forall result. ResultProcessor !(A.ParseMessageStream result) !(Either Text result -> IO ())

data UnaffiliatedResult =
  NotificationUnaffiliatedResult !Notification |
  ErrorMessageUnaffiliatedResult !ErrorMessage |
  ProtocolErrorUnaffiliatedResult !Text

loop :: IO Message -> IO (Maybe ResultProcessor) -> (UnaffiliatedResult -> IO ()) -> IO ()
loop fetchMessage fetchResultProcessor sendUnaffiliatedResult =
  forever $ do
    message <- fetchMessage
    fetchResult <- fetchResultProcessor
    case fetchResult of
      Just (ResultProcessor pms sendResult) ->
        do
          newFetchMessage <- backtrackFetch message fetchMessage
          parsingResult <- parseMessageStream newFetchMessage (interpretUnaffiliatedMessage sendUnaffiliatedResult) pms
          sendResult parsingResult
      Nothing ->
        interpretUnaffiliatedMessage sendUnaffiliatedResult message

{-|
Append one element to a fetching action.
-}
backtrackFetch :: a -> IO a -> IO (IO a)
backtrackFetch element fetch =
  do
    notFirstRef <- newIORef False
    return $ do
      notFirst <- readIORef notFirstRef
      if notFirst
        then fetch
        else do
          writeIORef notFirstRef True
          return element

parseMessageStream :: IO Message -> (Message -> IO ()) -> A.ParseMessageStream result -> IO (Either Text result)
parseMessageStream fetchMessage discardMessage (A.ParseMessageStream free) =
  runExceptT $ J.iterM interpretCompose free
  where
    interpretCompose (Compose (B.ParseMessage (I.Choosing typeFn))) =
      ExceptT $ fix $ \recur -> do
        Message type_ payload <- fetchMessage
        case typeFn type_ of
          Nothing -> do
            traceM ("Discarding a message of type \ESC[1m" <> H.string type_ <> "\ESC[0m")
            discardMessage (Message type_ payload)
            recur
          Just (ReaderT payloadFn) ->
            trace ("Consuming a message of type \ESC[1m" <> H.string type_ <> "\ESC[0m") $
            case payloadFn payload of
              Left (B.ParsingError context message) -> return (Left ((fromString . show) context <> ": " <> message))
              Right (Left sequenceError) -> return (Left sequenceError)
              Right (Right (ExceptT next)) -> next

interpretUnaffiliatedMessage :: (UnaffiliatedResult -> IO ()) -> Message -> IO ()
interpretUnaffiliatedMessage sendUnaffiliatedResult (Message type_ payload) =
  case unaffiliatedResultTypeFn type_ of
    Just payloadFn -> sendUnaffiliatedResult (payloadFn payload)
    Nothing -> return ()
  where
    F.ChooseMessage (I.Choosing unaffiliatedResultTypeFn) =
      fmap (either ProtocolErrorUnaffiliatedResult NotificationUnaffiliatedResult) F.notification <|>
      fmap (either ProtocolErrorUnaffiliatedResult ErrorMessageUnaffiliatedResult) F.error
