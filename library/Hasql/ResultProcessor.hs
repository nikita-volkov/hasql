module Hasql.ResultProcessor where

import Hasql.Prelude
import Hasql.Model
import qualified Hasql.MessageTypePredicates as G
import qualified Hasql.MessageTypeNames as H
import qualified Hasql.ParseDataRow as A
import qualified Data.Vector as B


newtype ResultProcessor result =
  ResultProcessor (Response -> IO Response -> (Response -> IO ()) -> IO result)

foldRows :: A.ParseDataRow row -> FoldM IO row result -> ResultProcessor (Either Text (result, Int))
foldRows (A.ParseDataRow rowLength vectorFn) (FoldM foldStep foldStart foldEnd) =
  ResultProcessor def
  where
    def firstResponse fetchResponse discardResponse =
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
            EmptyQueryResponse ->
              do
                result <- foldEnd state
                return (Right (result, 0))
            otherResponse ->
              do
                discardResponse otherResponse
                nextResponse <- fetchResponse
                processResponse state nextResponse

singleRow :: A.ParseDataRow row -> ResultProcessor (Either Text row)
singleRow (A.ParseDataRow rowLength vectorFn) =
  ResultProcessor def
  where
    def firstResponse fetchResponse discardResponse =
      processResponseWithoutRow firstResponse
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

rowsAffected :: ResultProcessor Int
rowsAffected =
  ResultProcessor def
  where
    def firstResponse fetchResponse discardResponse =
      processResponse firstResponse
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
