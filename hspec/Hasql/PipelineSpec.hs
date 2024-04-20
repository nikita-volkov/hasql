module Hasql.PipelineSpec (spec) where

import Hasql.TestingUtils.Statements.GenerateSeries qualified as GenerateSeries
import Hasql.TestingUtils.TestingDsl qualified as Dsl
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  describe "Normally" do
    describe "On prepared statements" do
      it "Collects results" do
        _result <-
          Dsl.runPipelineOnLocalDb
            $ (,)
            <$> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 1000}
            <*> GenerateSeries.pipeline True GenerateSeries.Params {start = 0, end = 1000}
        pending
    describe "On unprepared statements" do
      it "Works" do
        pending
  describe "When some part fails" do
    it "Works" do
      pending
