module Main where

import Prelude
import Bug
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Hasql.Connection as A
import qualified Hasql.Batch as J
import qualified Hasql.DecodeResult as B
import qualified Hasql.DecodeRow as C
import qualified Hasql.DecodePrimitive as D
import qualified Hasql.Statement as E
import qualified Data.Vector as H
import qualified Control.Foldl as I


main =
  connect >>= runTests

connect :: IO A.Connection
connect =
  do
    openingResult <- A.open (A.TCPConnectionSettings "localhost" 5432) "postgres" "" Nothing handleNotification
    case openingResult of
      Left error -> fail (showString "Can't connect: " (show error))
      Right connection -> return connection
  where
    handleNotification x =
      putStrLn ("Notification: " <> show x)

runTests :: A.Connection -> IO ()
runTests connection =
  defaultMain $
  testGroup "Tests with connection" $
  [
    let
      test :: (Eq result, Show result) => String -> Either A.Error result -> J.Batch result -> TestTree
      test name expectedResult query =
        testCase name $ do
          result <- A.batch connection query
          assertEqual "" expectedResult result 
      in
        testGroup "Batch" $
        [
          test "select 1" (Right 1) $
          J.statement (E.prepared "select 1" conquer (B.head (C.primitive D.int4))) ()
          ,
          test "select '1' and select 'true'" (Right (1, True)) $
          (,) <$>
          J.statement (E.prepared "select 1" conquer (B.head (C.primitive D.int4))) () <*>
          J.statement (E.prepared "select 'true' :: bool" conquer (B.head (C.primitive D.bool))) ()
          ,
          test "Error" (Left (A.BackendError "42703" "column \"abc\" does not exist")) $
          J.statement (E.prepared "select abc" conquer (B.head (C.primitive D.int4))) ()
          ,
          test "Errors in multiple queries" (Left (A.BackendError "42703" "column \"abc\" does not exist")) $
          J.statement (E.unprepared "select 1" conquer (B.head (C.primitive D.int4))) () *>
          J.statement (E.unprepared "select abc" conquer (B.head (C.primitive D.int4))) () *>
          J.statement (E.unprepared "select abc" conquer (B.head (C.primitive D.int4))) ()
          ,
          test "traverse" (Right [1,2,3]) $
          traverse (\template -> J.statement (E.prepared template conquer (B.head (C.primitive D.int4))) ()) $
          ["select 1", "select 2", "select 3"]
          ,
          test "Not a single row" (Left (A.DecodingError "Empty query")) $
          J.statement (E.prepared "" conquer (B.head (C.primitive D.int4))) ()
          ,
          testCaseInfo "Simultaneous result decoding and counting" $ pure "Pending"
        ]
    ,
    testCase "Failed prepared statement should be forgotten" $ do
      result1 <- A.batch connection $
        J.statement (E.prepared "fail 'Failed prepared statement 1'" conquer B.ignore) ()
      result2 <- A.batch connection $
        J.statement (E.prepared "fail 'Failed prepared statement 1'" conquer B.ignore) ()
      assertEqual "" (Left (A.BackendError "42601" "syntax error at or near \"fail\"")) result2
  ]
