module Hasql.Loops.Interpreter where

import Hasql.Prelude
import Hasql.Model
import qualified Hasql.MessageTypeNames as H
import qualified Hasql.ProcessResponses as C
import qualified Data.Vector as B


data ResultProcessor =
  forall result. ResultProcessor !(C.ProcessResponses result) !(result -> IO ())

loop :: IO Response -> IO (Maybe ResultProcessor) -> (Notification -> IO ()) -> IO ()
loop fetchResponse fetchResultProcessor sendNotification =
  forever $ do
    response <- fetchResponse
    fetchResult <- fetchResultProcessor
    case fetchResult of
      Just (ResultProcessor (C.ProcessResponses processResponses) sendResult) ->
        sendResult =<< processResponses response fetchResponse (interpretAsyncResponse sendNotification)
      Nothing ->
        interpretAsyncResponse sendNotification response

interpretAsyncResponse :: (Notification -> IO ()) -> Response -> IO ()
interpretAsyncResponse sendNotification =
  \case
    NotificationResponse a b c -> sendNotification (Notification a b c)
    _ -> return ()
