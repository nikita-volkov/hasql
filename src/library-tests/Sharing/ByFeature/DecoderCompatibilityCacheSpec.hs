module Sharing.ByFeature.DecoderCompatibilityCacheSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Errors qualified as Errors
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Test.Hspec
import Prelude

spec :: SpecWith (Text, Word16)
spec = parallel do
  byExecutor "Session" (Session.statement ())
  byExecutor "Pipeline" (Session.pipeline . Pipeline.statement ())

byExecutor ::
  Text ->
  (forall a. (Show a) => Statement.Statement () a -> Session.Session a) ->
  SpecWith (Text, Word16)
byExecutor executorName executor = do
  describe (toList executorName) do
    it "does not hide decoder mismatches from a previously verified statement" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let sql = "select 1::int8, 'text'::text"
            correctStatement =
              Statement.preparable
                sql
                mempty
                ( Decoders.singleRow
                    ( (,)
                        <$> Decoders.column (Decoders.nonNullable Decoders.int8)
                        <*> Decoders.column (Decoders.nonNullable Decoders.text)
                    )
                )
            mismatchingStatement =
              Statement.preparable
                sql
                mempty
                ( Decoders.singleRow
                    ( (,)
                        <$> Decoders.column (Decoders.nonNullable Decoders.int8)
                        <*> Decoders.column (Decoders.nonNullable Decoders.int8)
                    )
                )
        firstResult <- Connection.use connection (executor correctStatement)
        shouldBe firstResult (Right (1, "text"))
        secondResult <- Connection.use connection (executor mismatchingStatement)
        case secondResult of
          Left (Errors.StatementSessionError _ _ _ _ _ (Errors.UnexpectedColumnTypeStatementError column expected actual)) -> do
            shouldBe column 1
            (expected, actual) `shouldBe` (20, 25)
          Left err ->
            expectationFailure ("Unexpected type of error: " <> show err)
          result ->
            expectationFailure ("Not an error: " <> show result)
