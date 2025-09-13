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
        _ <- tryError $ Session.pipeline do
          Pipeline.statement
            ()
            ( Statement.Statement
                "select null :: int4"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                True
            )
        -- Now try running another pipeline to see if pipeline mode works again
        Session.pipeline do
          Pipeline.statement
            ()
            ( Statement.Statement
                "select 1"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                True
            )

      case result of
        Right _ ->
          pure ()
        Left result ->
          expectationFailure ("Unexpected error in second pipeline: " <> show result)

    it "Multiple consecutive pipeline failures are handled correctly" \connection -> do
      -- This test ensures that our fix handles multiple pipeline failures gracefully
      result <- Connection.use connection do
        _ <- tryError $ Session.pipeline do
          Pipeline.statement
            ()
            ( Statement.Statement
                "select null :: int4"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                True
            )
        _ <- tryError $ Session.pipeline do
          Pipeline.statement
            ()
            ( Statement.Statement
                "select 'not a number' :: int4"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                True
            )
        -- Final pipeline with succeeding statement - this should work
        Session.pipeline do
          Pipeline.statement
            ()
            ( Statement.Statement
                "select 1"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                True
            )

      case result of
        Right _ ->
          pure () -- Expected to succeed
        Left result ->
          expectationFailure ("Final pipeline failed unexpectedly: " <> show result)
