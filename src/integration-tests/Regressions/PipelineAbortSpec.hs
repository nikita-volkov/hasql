module Regressions.PipelineAbortSpec (spec) where

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
  describe "Failing pipeline" do
    it "Does not cause errors in the next pipeline" \connection -> do
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

    it "Handles failures within the same pipeline gracefully" \_ -> do
      pending
