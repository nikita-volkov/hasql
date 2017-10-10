module Hasql.Loops.Interpreter where

import Hasql.Prelude
import Hasql.Model
import qualified Hasql.MessageTypeNames as H
import qualified Hasql.InterpretResponses as C
import qualified Data.Vector as B


data ResultProcessor =
  forall result. ResultProcessor !(C.InterpretResponses result) !(Either Error result -> IO ())

loop :: IO Response -> IO (Maybe ResultProcessor) -> (Notification -> IO ()) -> IO ()
loop fetchResponse fetchResultProcessor sendNotification =
  forever $ do
    response <- fetchResponse
    fetchResult <- fetchResultProcessor
    case fetchResult of
      Just (ResultProcessor (C.InterpretResponses processResponses) sendResult) ->
        sendResult =<< processResponses response fetchResponse (interpretAsyncResponse sendNotification)
      Nothing ->
        interpretAsyncResponse sendNotification response

interpretAsyncResponse :: (Notification -> IO ()) -> Response -> IO ()
interpretAsyncResponse sendNotification response =
  case response of
    NotificationResponse a b c -> sendNotification (Notification a b c)
    _ -> return ()
