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
      -- Test same pattern as existing working tests - failure in a multi-statement pipeline
      result <- Connection.use connection do
        _ <- tryError $ Session.pipeline do
          (,) 
            <$> Pipeline.statement
                  ()
                  ( Statement.Statement
                      "select null :: int4"
                      mempty
                      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                      True
                  )
            <*> Pipeline.statement
                  ()
                  ( Statement.Statement
                      "select 1"
                      mempty
                      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                      True
                  )
        -- Now test that the connection is still usable with a regular session
        Session.statement
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
          expectationFailure ("Unexpected error in regular session after pipeline failure: " <> show result)

    it "Consecutive separate pipelines work after failure" \connection -> do
      -- This tests the specific issue in the bug report - consecutive separate pipelines
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
        -- This should work but currently fails with PipelineAbort
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
          expectationFailure ("Consecutive pipeline failed: " <> show result)
