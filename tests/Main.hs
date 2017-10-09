module Main where

import Prelude
import Bug
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Hasql.Connection as A
import qualified Hasql.Query as J
import qualified Hasql.Model as E
import qualified Data.Vector as H
import qualified Control.Foldl as I
import qualified Hasql.DecodeResult as B
import qualified Hasql.DecodeRow as C
import qualified Hasql.DecodePrimitive as D


main =
  connect >>= runTests

connect :: IO A.Connection
connect =
  do
    openingResult <- A.open (A.TCPConnectionSettings "localhost" 5432) "postgres" "" Nothing handleErrorOrNotification
    case openingResult of
      Left error -> fail (showString "Can't connect: " (show error))
      Right connection -> return connection
  where
    handleErrorOrNotification x =
      putStrLn ("Async event: " <> show x)

runTests :: A.Connection -> IO ()
runTests connection =
  defaultMain $
  testGroup "Tests with connection" $
  [
    let
      test :: (Eq result, Show result) => String -> Either E.Error result -> J.Query result -> TestTree
      test name expectedResult query =
        testCase name $ do
          result <- A.query connection query
          assertEqual "" expectedResult result 
      in
        testGroup "Query" $
        [
          test "select 1" (Right 1) $
          J.preparedStatement "select 1" mempty (B.row (C.nonNullPrimitive D.int8))
          ,
          test "select '1' and select 'true'" (Right (1, True)) $
          (,) <$>
          J.preparedStatement "select 1" mempty (B.row (C.nonNullPrimitive D.int8)) <*>
          J.preparedStatement "select 'true' :: bool" mempty (B.row (C.nonNullPrimitive D.bool))
          ,
          test "Error" (Left (E.BackendError "42703" "column \"abc\" does not exist")) $
          J.preparedStatement "select abc" mempty (B.row (C.nonNullPrimitive D.int8))
          ,
          test "Errors in multiple queries" (Left (E.BackendError "42703" "column \"abc\" does not exist")) $
          J.preparedStatement "select 1" mempty (B.row (C.nonNullPrimitive D.int8)) *>
          J.preparedStatement "select abc" mempty (B.row (C.nonNullPrimitive D.int8)) *>
          J.preparedStatement "select abc" mempty (B.row (C.nonNullPrimitive D.int8))
          ,
          test "traverse" (Right [1,2,3]) $
          traverse (\template -> J.preparedStatement template mempty (B.row (C.nonNullPrimitive D.int8))) $
          ["select 1", "select 2", "select 3"]
          ,
          testCaseInfo "Simultaneous result decoding and counting" $ pure "Pending"
        ]
  ]
