module Regressions.PreparedStatementAlreadyExistsSpec (spec) where

import Data.Either
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec
import TestingKit.Testcontainers qualified as Testcontainers
import Prelude

spec :: Spec
spec = around Testcontainers.withConnection do
  describe "Session" do
    it "Failing statements don't cause misses in updates of the prepared statement cache" \connection -> do
      -- Run an intentionally failing prepared statement to set the condition of the bug.
      result <- Connection.use connection do
        Session.statement
          ()
          ( Statement.Statement
              "select null"
              mempty
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
              True
          )
      shouldBe (isLeft result) True
      -- Run a succeeding prepared statement to see if the cache is still in a good state.
      result <- Connection.use connection do
        Session.statement
          ()
          ( Statement.Statement
              "select 1"
              mempty
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
              True
          )
      -- If there is an error the cache got corrupted.
      case result of
        Right _ ->
          pure ()
        Left result ->
          expectationFailure ("Unexpected error: " <> show result)

  describe "Pipeline" do
    it "Failing pipeline statements don't cause misses in updates of the prepared statement cache" \connection -> do
      -- Run an intentionally failing prepared statement in a pipeline to set the condition of the bug.
      result <- Connection.use connection do
        Session.pipeline do
          Pipeline.statement
            ()
            ( Statement.Statement
                "select null :: int4"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                True
            )
      case result of
        Right val ->
          expectationFailure ("First statement succeeded unexpectedly: " <> show val)
        Left err -> case err of
          Session.QueryError _ _ (Session.ResultError (Session.RowError 0 0 Session.UnexpectedNull)) ->
            pure ()
          _ ->
            expectationFailure ("Unexpected error: " <> show err)

      -- Run a succeeding prepared statement in a pipeline to see if the cache is still in a good state.
      putStrLn "Running second statement"
      result <- Connection.use connection do
        Session.pipeline do
          Pipeline.statement
            ()
            ( Statement.Statement
                "select 1"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                True
            )
      -- If there is an error the cache got corrupted.
      case result of
        Right _ ->
          pure ()
        Left result ->
          expectationFailure ("Unexpected error: " <> show result)

    it "Multiple consecutive pipeline failures are handled correctly" \connection -> do
      -- This test ensures that our fix handles multiple pipeline failures gracefully
      -- First pipeline with failing statement
      result1 <- Connection.use connection do
        Session.pipeline do
          Pipeline.statement
            ()
            ( Statement.Statement
                "select null :: int4"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                True
            )
      case result1 of
        Right val ->
          expectationFailure ("First pipeline succeeded unexpectedly: " <> show val)
        Left _ ->
          pure () -- Expected to fail

      -- Second pipeline with failing statement
      result2 <- Connection.use connection do
        Session.pipeline do
          Pipeline.statement
            ()
            ( Statement.Statement
                "select 'not a number' :: int4"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                True
            )
      case result2 of
        Right val ->
          expectationFailure ("Second pipeline succeeded unexpectedly: " <> show val)
        Left _ ->
          pure () -- Expected to fail

      -- Third pipeline with succeeding statement - this should work
      result3 <- Connection.use connection do
        Session.pipeline do
          Pipeline.statement
            ()
            ( Statement.Statement
                "select 1"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                True
            )
      case result3 of
        Right _ ->
          pure () -- Expected to succeed
        Left result ->
          expectationFailure ("Third pipeline failed unexpectedly: " <> show result)
