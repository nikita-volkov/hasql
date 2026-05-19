module Sharing.ByFeature.PreparedStatementCacheSpec (spec) where

import Data.Either
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
spec = do
  describe "Session" do
    it "Failing statements don't cause misses in updates of the prepared statement cache" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        -- Run an intentionally failing prepared statement to set the condition of the bug.
        result <- Connection.use connection do
          Session.statement
            ()
            ( Statement.preparable
                "select null"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
            )
        shouldBe (isLeft result) True
        -- Run a succeeding prepared statement to see if the cache is still in a good state.
        result <- Connection.use connection do
          Session.statement
            ()
            ( Statement.preparable
                "select 1"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
            )
        -- If there is an error the cache got corrupted.
        case result of
          Right _ ->
            pure ()
          Left result ->
            expectationFailure ("Unexpected error: " <> show result)

    it "Syntax errors in prepared statements don't corrupt the cache for subsequent uses of the same statement" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let brokenStatement =
              Statement.preparable
                "S"
                mempty
                Decoders.noResult
        -- First run: syntax error.
        result1 <- Connection.use connection do
          Session.statement () brokenStatement
        error1 <- case result1 of
          Left error1 -> pure error1
          Right _ -> fail "First run unexpectedly succeeded"

        -- Second run of the same statement: should also produce a syntax error,
        -- not "prepared statement does not exist".
        result2 <- Connection.use connection do
          Session.statement () brokenStatement
        error2 <- case result2 of
          Left error2 -> pure error2
          Right _ -> fail "Second run unexpectedly succeeded"
        shouldBe error2 error1

  describe "Pipeline" do
    it "Failing pipeline statements don't cause misses in updates of the prepared statement cache" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        -- Run an intentionally failing prepared statement in a pipeline to set the condition of the bug.
        result <- Connection.use connection do
          Session.pipeline do
            Pipeline.statement
              ()
              ( Statement.preparable
                  "select null :: int4"
                  mempty
                  (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
              )
        case result of
          Right val ->
            expectationFailure ("First statement succeeded unexpectedly: " <> show val)
          Left _ ->
            pure ()

        -- Run a succeeding prepared statement in a pipeline to see if the cache is still in a good state.
        result <- Connection.use connection do
          Session.pipeline do
            Pipeline.statement
              ()
              ( Statement.preparable
                  "select 1"
                  mempty
                  (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
              )
        -- If there is an error the cache got corrupted.
        case result of
          Right _ ->
            pure ()
          Left result ->
            expectationFailure ("Unexpected error: " <> show result)

    it "Syntax errors in pipeline prepared statements don't corrupt the cache for subsequent uses of the same statement" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let brokenStatement =
              Statement.preparable
                "S"
                mempty
                Decoders.noResult
        -- First run: syntax error.
        result1 <- Connection.use connection do
          Session.pipeline (Pipeline.statement () brokenStatement)
        shouldBe (isLeft result1) True
        -- Second run of the same statement: should also produce a syntax error,
        -- not "prepared statement does not exist".
        result2 <- Connection.use connection do
          Session.pipeline (Pipeline.statement () brokenStatement)
        case result2 of
          Left (Errors.StatementSessionError _ _ _ _ _ (Errors.ServerStatementError (Errors.ServerError "42601" _ _ _ _))) ->
            pure ()
          Left other ->
            expectationFailure ("Unexpected error on second run: " <> show other)
          Right _ ->
            expectationFailure "Second run unexpectedly succeeded"

    it "A pipeline with successful statements followed by a broken one can be retried without 'already exists' errors" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let ok1 = Statement.preparable "select 1" mempty (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
            ok2 = Statement.preparable "select 2" mempty (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
            broken = Statement.preparable "S" mempty Decoders.noResult
        -- First run: pipeline with two OK statements and a broken one at the end.
        result1 <- Connection.use connection do
          Session.pipeline do
            (,,)
              <$> Pipeline.statement () ok1
              <*> Pipeline.statement () ok2
              <*> Pipeline.statement () broken
        error1 <- case result1 of
          Left error1 -> pure error1
          Right _ -> fail "First run unexpectedly succeeded"

        -- Second run of the same pipeline: must fail with the SAME syntax error,
        -- not "prepared statement already exists".
        result2 <- Connection.use connection do
          Session.pipeline do
            (,,)
              <$> Pipeline.statement () ok1
              <*> Pipeline.statement () ok2
              <*> Pipeline.statement () broken
        error2 <- case result2 of
          Left error2 -> pure error2
          Right _ -> fail "Second run unexpectedly succeeded"
        shouldBe error2 error1

        -- Also, a standalone valid statement should still work afterwards.
        result3 <- Connection.use connection do
          Session.statement () ok1
        case result3 of
          Right val -> val `shouldBe` 1
          Left err -> expectationFailure ("Unexpected error on standalone statement: " <> show err)

    it "A pipeline with a broken statement in the middle can be retried without 'already exists' errors" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let ok1 = Statement.preparable "select 1" mempty (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
            broken = Statement.preparable "S" mempty Decoders.noResult
            ok2 = Statement.preparable "select 2" mempty (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
        -- First run: pipeline with broken statement in the middle.
        result1 <- Connection.use connection do
          Session.pipeline do
            (,,)
              <$> Pipeline.statement () ok1
              <*> Pipeline.statement () broken
              <*> Pipeline.statement () ok2
        shouldBe (isLeft result1) True

        -- Second run of the same pipeline: must fail with the same syntax error.
        result2 <- Connection.use connection do
          Session.pipeline do
            (,,)
              <$> Pipeline.statement () ok1
              <*> Pipeline.statement () broken
              <*> Pipeline.statement () ok2
        case result2 of
          Left (Errors.StatementSessionError _ _ _ _ _ (Errors.ServerStatementError (Errors.ServerError "42601" _ _ _ _))) ->
            pure ()
          Left other ->
            expectationFailure ("Unexpected error on second run: " <> show other)
          Right _ ->
            expectationFailure "Second run unexpectedly succeeded"

        -- Standalone valid statements should still work afterwards.
        result3 <- Connection.use connection do
          Session.statement () ok1
        case result3 of
          Right val -> val `shouldBe` 1
          Left err -> expectationFailure ("Unexpected error on standalone ok1: " <> show err)
        result4 <- Connection.use connection do
          Session.statement () ok2
        case result4 of
          Right val -> val `shouldBe` 2
          Left err -> expectationFailure ("Unexpected error on standalone ok2: " <> show err)
