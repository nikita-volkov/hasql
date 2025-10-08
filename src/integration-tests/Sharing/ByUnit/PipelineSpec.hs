module Sharing.ByUnit.PipelineSpec (spec) where

import Data.Either
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Errors qualified as Errors
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Test.Hspec
import TestingKit.Statements.BrokenSyntax qualified as BrokenSyntax
import TestingKit.Statements.GenerateSeries qualified as GenerateSeries
import TestingKit.Statements.WrongDecoder qualified as WrongDecoder
import TestingKit.TestingDsl qualified as Dsl
import Prelude

spec :: SpecWith (Text, Word16)
spec = do
  describe "Single-statement" do
    describe "Unprepared" do
      it "Collects results and sends params" \config -> do
        Scripts.onConnection config \connection -> do
          result <-
            (Connection.use connection . Session.pipeline)
              $ GenerateSeries.pipeline False GenerateSeries.Params {start = 0, end = 2}
          shouldBe result (Right [0 .. 2])

    describe "Prepared" do
      it "Collects results and sends params" \config -> do
        Scripts.onConnection config \connection -> do
          result <-
            (Connection.use connection . Session.pipeline)
              $ GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
          shouldBe result (Right [0 .. 2])

  describe "Multi-statement" do
    describe "On unprepared statements" do
      it "Collects results and sends params" \config -> do
        Scripts.onConnection config \connection -> do
          result <-
            (Connection.use connection . Session.pipeline)
              $ replicateM 2
              $ GenerateSeries.pipeline False GenerateSeries.Params {start = 0, end = 2}
          shouldBe result (Right [[0 .. 2], [0 .. 2]])

    describe "On prepared statements" do
      it "Collects results and sends params" \config -> do
        Scripts.onConnection config \connection -> do
          result <-
            (Connection.use connection . Session.pipeline)
              $ replicateM 2
              $ GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
          shouldBe result (Right [[0 .. 2], [0 .. 2]])

    describe "When a part in the middle fails" do
      describe "With query error" do
        it "Captures the error" \config -> do
          Scripts.onConnection config \connection -> do
            result <-
              (Connection.use connection . Session.pipeline)
                $ (,,)
                <$> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
                <*> BrokenSyntax.pipeline True BrokenSyntax.Params {start = 0, end = 2}
                <*> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
            case result of
              Left (Errors.StatementSessionError _ _ _ _ _ (Errors.ExecutionStatementError _)) -> pure ()
              _ -> expectationFailure $ "Unexpected result: " <> show result

        it "Leaves the connection usable" \config -> do
          Scripts.onConnection config \connection -> do
            result <-
              Connection.use connection do
                _ <-
                  tryError
                    $ Dsl.runPipelineInSession
                    $ (,,)
                    <$> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
                    <*> BrokenSyntax.pipeline True BrokenSyntax.Params {start = 0, end = 2}
                    <*> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
                GenerateSeries.session True GenerateSeries.Params {start = 0, end = 0}
            shouldBe result (Right [0])

      describe "With decoding error" do
        it "Captures the error" \config -> do
          Scripts.onConnection config \connection -> do
            result <-
              (Connection.use connection . Session.pipeline)
                $ (,,)
                <$> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
                <*> WrongDecoder.pipeline True WrongDecoder.Params {start = 0, end = 2}
                <*> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
            case result of
              Left (Errors.StatementSessionError _ _ _ _ _ (Errors.UnexpectedColumnTypeStatementError {})) -> pure ()
              _ -> expectationFailure $ "Unexpected result: " <> show result

        it "Leaves the connection usable" \config -> do
          Scripts.onConnection config \connection -> do
            result <-
              Connection.use connection do
                _ <-
                  tryError
                    $ Dsl.runPipelineInSession
                    $ (,,)
                    <$> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
                    <*> WrongDecoder.pipeline True WrongDecoder.Params {start = 0, end = 2}
                    <*> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
                GenerateSeries.session True GenerateSeries.Params {start = 0, end = 0}
            shouldBe result (Right [0])

  describe "Failing pipeline" do
    it "Does not cause errors in the next pipeline" \config -> do
      Scripts.onConnection config \connection -> do
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

    it "Handles failures within the same pipeline gracefully" \config -> do
      Scripts.onConnection config \connection -> do
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
              <* Pipeline.statement
                ()
                ( Statement.Statement
                    "select 1"
                    mempty
                    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                    True
                )
        case result of
          Right val ->
            expectationFailure ("First statement succeeded unexpectedly: " <> show val)
          Left _ ->
            pure ()
