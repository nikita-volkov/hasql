module Hasql.ResultProcessor where

import Hasql.Prelude
import Hasql.Model
import qualified Hasql.MessageTypePredicates as G
import qualified Hasql.MessageTypeNames as H
import qualified Hasql.ParseDataRow as A
import qualified Data.Vector as B


newtype ResultProcessor =
  ResultProcessor (Response -> IO Response -> (Response -> IO ()) -> IO ())

foldRows :: A.ParseDataRow row -> FoldM IO row result -> (Either Text (result, Int) -> IO ()) -> ResultProcessor
foldRows (A.ParseDataRow rowLength vectorFn) (FoldM foldStep foldStart foldEnd) sendResult =
  ResultProcessor def
  where
    def firstResponse fetchResponse discardResponse =
      do
        initialState <- foldStart
        result <- processResponse initialState firstResponse
        sendResult result
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
            EmptyQueryResponse ->
              do
                result <- foldEnd state
                return (Right (result, 0))
            otherResponse ->
              do
                discardResponse otherResponse
                nextResponse <- fetchResponse
                processResponse state nextResponse

singleRow :: A.ParseDataRow row -> (Either Text row -> IO ()) -> ResultProcessor
singleRow (A.ParseDataRow rowLength vectorFn) sendResult =
  ResultProcessor def
  where
    def firstResponse fetchResponse discardResponse =
      sendResult =<< processResponseWithoutRow firstResponse
      where
        processResponseWithoutRow =
          \case
            DataRowResponse values ->
              if B.length values == rowLength
                then case vectorFn values 0 of
                  Left error -> return (Left error)
                  Right row -> do
                    nextResponse <- fetchResponse
                    processResponseWithRow row nextResponse
                else return (Left (fromString
                  (showString "Invalid amount of columns: "
                    (shows (B.length values)
                      (showString ", expecting "
                        (show rowLength))))))
            CommandCompleteResponse _ ->
              return (Left "Not a single row")
            EmptyQueryResponse ->
              return (Left "Empty query")
            otherResponse ->
              do
                discardResponse otherResponse
                nextResponse <- fetchResponse
                processResponseWithoutRow nextResponse
        processResponseWithRow row =
          \case
            DataRowResponse _ ->
              do
                nextResponse <- fetchResponse
                processResponseWithRow row nextResponse
            CommandCompleteResponse _ ->
              return (Right row)
            otherResponse ->
              do
                discardResponse otherResponse
                nextResponse <- fetchResponse
                processResponseWithRow row nextResponse

rowsAffected :: (Int -> IO ()) -> ResultProcessor
rowsAffected sendResult =
  ResultProcessor def
  where
    def firstResponse fetchResponse discardResponse =
      sendResult =<< processResponse firstResponse
      where
        processResponse =
          \case
            CommandCompleteResponse amount -> return amount
            DataRowResponse _ -> fetchResponse >>= processResponse
            EmptyQueryResponse -> return 0
            otherResponse -> do
              discardResponse otherResponse
              nextResponse <- fetchResponse
              processResponse nextResponse
