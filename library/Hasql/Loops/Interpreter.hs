module Hasql.Loops.Interpreter where

import Hasql.Prelude
import Hasql.Model
import qualified Hasql.MessageTypePredicates as G
import qualified Hasql.MessageTypeNames as H
import qualified Hasql.ParseDataRow as A
import qualified Data.Vector as B


data ResultProcessor =
  forall row result. RowsFoldResultProcessor !(A.ParseDataRow row) !(FoldM IO row result) !(Either Text (result, Int) -> IO ()) |
  RowsAffectedResultProcessor !(Either Text Int -> IO ()) |
  AuthenticationResultProcessor !(Either Text AuthenticationResult -> IO ())

loop :: IO Response -> IO (Maybe ResultProcessor) -> (Notification -> IO ()) -> IO ()
loop fetchResponse fetchResultProcessor sendNotification =
  forever $ do
    response <- fetchResponse
    fetchResult <- fetchResultProcessor
    case fetchResult of
      Just resultProcessor -> case resultProcessor of
        RowsFoldResultProcessor pdr fold sendResult ->
          sendResult =<< foldRows response fetchResponse (interpretAsyncResponse sendNotification) pdr fold
      Nothing ->
        interpretAsyncResponse sendNotification response

-- {-|
-- Append one element to a fetching action.
-- -}
-- backtrackFetch :: a -> IO a -> IO (IO a)
-- backtrackFetch element fetch =
--   do
--     notFirstRef <- newIORef False
--     return $ do
--       notFirst <- readIORef notFirstRef
--       if notFirst
--         then fetch
--         else do
--           writeIORef notFirstRef True
--           return element

foldRows :: Response -> IO Response -> (Response -> IO ()) -> A.ParseDataRow row -> FoldM IO row result -> IO (Either Text (result, Int))
foldRows firstResponse fetchResponse discardResponse (A.ParseDataRow rowLength vectorFn) (FoldM foldStep foldStart foldEnd) =
  do
    initialState <- foldStart
    processResponse initialState firstResponse
  where
    processResponse !state =
      \case
        DataRowResponse values ->
          if B.length values == rowLength
            then case vectorFn values 0 of
              Left error -> return (Left error)
              Right row -> do
                nextState <- foldStep state row
                nextResponse <- fetchResponse
                processResponse nextState nextResponse
            else return (Left (fromString
              (showString "Invalid amount of columns: "
                (shows (B.length values)
                  (showString ", expecting "
                    (show rowLength))))))
        CommandCompleteResponse amount ->
          do
            result <- foldEnd state
            return (Right (result, amount))
        

-- parseResponseStream :: IO Response -> (Response -> IO ()) -> A.ParseResponseStream result -> IO (Either Text result)
-- parseResponseStream fetchResponse discardResponse (A.ParseResponseStream free) =
--   runExceptT $ J.iterM interpretCompose free
--   where
--     interpretCompose (Compose (B.ParseResponse (I.Choosing typeFn))) =
--       ExceptT $ fix $ \recur -> do
--         Response type_ payload <- fetchResponse
--         case typeFn type_ of
--           Nothing -> do
--             discardResponse (Response type_ payload)
--             recur
--           Just (ReaderT payloadFn) ->
--             case payloadFn payload of
--               Left (B.ParsingError context response) -> return (Left ((fromString . show) context <> ": " <> response))
--               Right (Left sequenceError) -> return (Left sequenceError)
--               Right (Right (ExceptT next)) -> next

interpretAsyncResponse :: (Notification -> IO ()) -> Response -> IO ()
interpretAsyncResponse sendNotification =
  \case
    NotificationResponse a b c -> sendNotification (Notification a b c)
    _ -> return ()
