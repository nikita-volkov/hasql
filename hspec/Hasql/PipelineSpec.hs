module Hasql.PipelineSpec (spec) where

import Hasql.Session qualified as Session
import Hasql.TestingKit.Statements.BrokenSyntax qualified as BrokenSyntax
import Hasql.TestingKit.Statements.GenerateSeries qualified as GenerateSeries
import Hasql.TestingKit.Statements.WrongDecoder qualified as WrongDecoder
import Hasql.TestingKit.TestcontainersHelpers qualified as TestcontainersHelpers
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  describe "Single-statement" do
    describe "Unprepared" do
      it "Collects results and sends params" do
        result <-
          TestcontainersHelpers.runPipelineWithTestcontainers
            $ GenerateSeries.pipeline False GenerateSeries.Params {start = 0, end = 2}
        shouldBe result (Right [0 .. 2])

    describe "Prepared" do
      it "Collects results and sends params" do
        result <-
          TestcontainersHelpers.runPipelineWithTestcontainers
            $ GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
        shouldBe result (Right [0 .. 2])

  describe "Multi-statement" do
    describe "On unprepared statements" do
      it "Collects results and sends params" do
        result <-
          TestcontainersHelpers.runPipelineWithTestcontainers
            $ replicateM 2
            $ GenerateSeries.pipeline False GenerateSeries.Params {start = 0, end = 2}
        shouldBe result (Right [[0 .. 2], [0 .. 2]])

    describe "On prepared statements" do
      it "Collects results and sends params" do
        result <-
          TestcontainersHelpers.runPipelineWithTestcontainers
            $ replicateM 2
            $ GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
        shouldBe result (Right [[0 .. 2], [0 .. 2]])

    describe "When a part in the middle fails" do
      describe "With query error" do
        it "Captures the error" do
          result <-
            TestcontainersHelpers.runPipelineWithTestcontainers
              $ (,,)
              <$> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
              <*> BrokenSyntax.pipeline True BrokenSyntax.Params {start = 0, end = 2}
              <*> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
          case result of
            Left (TestcontainersHelpers.SessionError (Session.QueryError _ _ _)) -> pure ()
            _ -> expectationFailure $ "Unexpected result: " <> show result

        it "Leaves the connection usable" do
          result <-
            TestcontainersHelpers.runSessionWithTestcontainers do
              _ <-
                tryError
                  $ Session.pipeline
                  $ (,,)
                  <$> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
                  <*> BrokenSyntax.pipeline True BrokenSyntax.Params {start = 0, end = 2}
                  <*> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
              GenerateSeries.session True GenerateSeries.Params {start = 0, end = 0}
          shouldBe result (Right [0])

      describe "With decoding error" do
        it "Captures the error" do
          result <-
            TestcontainersHelpers.runPipelineWithTestcontainers
              $ (,,)
              <$> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
              <*> WrongDecoder.pipeline True WrongDecoder.Params {start = 0, end = 2}
              <*> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
          case result of
            Left (TestcontainersHelpers.SessionError (Session.QueryError _ _ _)) -> pure ()
            _ -> expectationFailure $ "Unexpected result: " <> show result

        it "Leaves the connection usable" do
          result <-
            TestcontainersHelpers.runSessionWithTestcontainers do
              _ <-
                tryError
                  $ Session.pipeline
                  $ (,,)
                  <$> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
                  <*> WrongDecoder.pipeline True WrongDecoder.Params {start = 0, end = 2}
                  <*> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
              GenerateSeries.session True GenerateSeries.Params {start = 0, end = 0}
          shouldBe result (Right [0])
