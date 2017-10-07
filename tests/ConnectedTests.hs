module ConnectedTests where

import Prelude
import Bug
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Hasql.Connection as A
import qualified Hasql.Query as J
import qualified Data.Vector as H
import qualified Control.Foldl as I
import qualified Hasql.DecodeResult as B
import qualified Hasql.DecodeRow as C
import qualified Hasql.DecodePrimitive as D


tests :: TestTree
tests =
  withResource acquire release use
  where
    acquire =
      A.open (A.TCPConnectionSettings "localhost" 5432) "postgres" "" Nothing handleErrorOrNotification
      where
        handleErrorOrNotification x =
          putStrLn ("Async event: " <> show x)
    release =
      either (fail . show) (A.close)
    use getConnection =
      case unsafePerformIO getConnection of
        Left error -> testGroup "No tests" []
        Right connection -> testsWithConnection connection

testsWithConnection :: A.Connection -> TestTree
testsWithConnection connection =
  testGroup "Tests with connection"
  [
    testCase "Combined query of select '1' and select 'true'" $ 
    let
      query :: J.Query (Int64, Bool)
      query = (,) <$> selectOne <*> selectTrue
        where
          selectOne = J.preparedStatement "select 1 :: int8" mempty (B.row (C.nonNullPrimitive D.int8))
          selectTrue = J.preparedStatement "select 'true' :: bool" mempty (B.row (C.nonNullPrimitive D.bool))
      in do
        result <- A.query connection query
        assertEqual "" (Right (1, True)) result
    ,
    testCase "Simultaneous result decoding and counting" $
    return ()
  ]

