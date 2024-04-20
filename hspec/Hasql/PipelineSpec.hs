module Hasql.PipelineSpec (spec) where

import Hasql.TestingUtils.Statements.GenerateSeries qualified as GenerateSeries
import Hasql.TestingUtils.TestingDsl qualified as Dsl
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  describe "Single-statement" do
    describe "Unprepared" do
      it "Collects results and sends params" do
        result <-
          Dsl.runPipelineOnLocalDb
            $ GenerateSeries.pipeline False GenerateSeries.Params {start = 0, end = 2}
        shouldBe result (Right [0 .. 2])

    describe "Prepared and sends params" do
      fit "Collects results and sends params" do
        result <-
          Dsl.runPipelineOnLocalDb
            $ GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
        shouldBe result (Right [0 .. 2])

  describe "Normally" do
    describe "On prepared statements" do
      it "Collects results and sends params" do
        result <-
          Dsl.runPipelineOnLocalDb
            $ replicateM 2
            $ GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 2}
        shouldBe result (Right [[0 .. 2], [0 .. 2]])

    describe "On unprepared statements" do
      it "Collects results and sends params" do
        result <-
          Dsl.runPipelineOnLocalDb
            $ replicateM 2
            $ GenerateSeries.pipeline False GenerateSeries.Params {start = 0, end = 2}
        shouldBe result (Right [[0 .. 2], [0 .. 2]])

  describe "When some part fails" do
    it "Works" do
      pending
