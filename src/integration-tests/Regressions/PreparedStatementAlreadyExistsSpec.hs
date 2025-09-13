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
                "select 1/0"  -- This causes a PostgreSQL error
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                True  -- Preparable
            )
      case result of
        Right val ->
          expectationFailure ("First statement succeeded unexpectedly: " <> show val)
        Left err -> case err of
          Session.QueryError _ _ (Session.ResultError (Session.ServerError _ _ _ _ _ _)) ->
            pure ()  -- Accept any server error
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
                True  -- Preparable
            )
      -- If there is an error the cache got corrupted.
      case result of
        Right _ ->
          pure ()
        Left result ->
          expectationFailure ("Unexpected error: " <> show result)
