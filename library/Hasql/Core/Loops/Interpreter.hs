module Hasql.Core.Loops.Interpreter where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.MessageTypeNames as H
import qualified Hasql.Core.InterpretResponses as C
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
        do
          newFetchResponse <- backtrackFetch response fetchResponse
          sendResult =<< processResponses newFetchResponse (interpretAsyncResponse sendNotification)
      Nothing ->
        interpretAsyncResponse sendNotification response

interpretAsyncResponse :: (Notification -> IO ()) -> Response -> IO ()
interpretAsyncResponse sendNotification response =
  case response of
    NotificationResponse a b c -> sendNotification (Notification a b c)
    _ -> return ()

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
