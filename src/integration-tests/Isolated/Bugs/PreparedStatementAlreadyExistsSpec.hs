module Isolated.Bugs.PreparedStatementAlreadyExistsSpec (spec) where

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
spec = Testcontainers.aroundSpecWithConnection True do
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
        Left _ ->
          pure ()

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
