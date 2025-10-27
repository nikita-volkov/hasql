module Sharing.ByUnit.Encoders.IntervalSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Test.Hspec
import Prelude

spec :: SpecWith (Text, Word16)
spec = do
  describe "Interval Encoders" do
    it "encodes intervals correctly" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.preparable
                "select $1 = interval '10 seconds'"
                (Encoders.param (Encoders.nonNullable Encoders.interval))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        result <- Connection.use connection (Session.statement (10 :: DiffTime) statement)
        result `shouldBe` Right True

    it "roundtrips intervals correctly" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.preparable
                "select $1"
                (Encoders.param (Encoders.nonNullable Encoders.interval))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.interval)))
        result <- Connection.use connection (Session.statement (10 :: DiffTime) statement)
        result `shouldBe` Right (10 :: DiffTime)
