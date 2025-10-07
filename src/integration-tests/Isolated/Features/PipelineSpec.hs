module Isolated.Features.PipelineSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Errors qualified as Errors
import Hasql.Session qualified as Session
import Test.Hspec
import TestingKit.Statements.BrokenSyntax qualified as BrokenSyntax
import TestingKit.Statements.GenerateSeries qualified as GenerateSeries
import TestingKit.Statements.WrongDecoder qualified as WrongDecoder
import TestingKit.Testcontainers qualified as Testcontainers
import TestingKit.TestingDsl qualified as Dsl
import Prelude

spec :: Spec
spec = Testcontainers.aroundSpecWithConnection False do
  describe "Single-statement" do
    describe "Unprepared" do
      it "Collects results and sends params" \connection -> do
        result <-
          (Connection.use connection . Session.pipeline)
            $ GenerateSeries.pipeline False GenerateSeries.Params {start = 0, end = 2}
        shouldBe result (Right [0 .. 2])

    describe "Prepared" do
      it "Collects results and sends params" \connection -> do
        result <-
          (Connection.use connection . Session.pipeline)
            $ GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
        shouldBe result (Right [0 .. 2])

  describe "Multi-statement" do
    describe "On unprepared statements" do
      it "Collects results and sends params" \connection -> do
        result <-
          (Connection.use connection . Session.pipeline)
            $ replicateM 2
            $ GenerateSeries.pipeline False GenerateSeries.Params {start = 0, end = 2}
        shouldBe result (Right [[0 .. 2], [0 .. 2]])

    describe "On prepared statements" do
      it "Collects results and sends params" \connection -> do
        result <-
          (Connection.use connection . Session.pipeline)
            $ replicateM 2
            $ GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
        shouldBe result (Right [[0 .. 2], [0 .. 2]])

    describe "When a part in the middle fails" do
      describe "With query error" do
        it "Captures the error" \connection -> do
          result <-
            (Connection.use connection . Session.pipeline)
              $ (,,)
              <$> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
              <*> BrokenSyntax.pipeline True BrokenSyntax.Params {start = 0, end = 2}
              <*> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
          case result of
            Left (Errors.StatementSessionError _ _ _ _ _ (Errors.ExecutionStatementError _)) -> pure ()
            _ -> expectationFailure $ "Unexpected result: " <> show result

        it "Leaves the connection usable" \connection -> do
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
        it "Captures the error" \connection -> do
          result <-
            (Connection.use connection . Session.pipeline)
              $ (,,)
              <$> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
              <*> WrongDecoder.pipeline True WrongDecoder.Params {start = 0, end = 2}
              <*> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
          case result of
            Left (Errors.StatementSessionError _ _ _ _ _ (Errors.ResultCellStatementError {})) -> pure ()
            _ -> expectationFailure $ "Unexpected result: " <> show result

        it "Leaves the connection usable" \connection -> do
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
