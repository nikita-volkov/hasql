module Sharing.ByUnit.Encoders.UnknownSpec (spec) where

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
  describe "Unknown Type Encoders" do
    it "handles unknown type encoding" \config -> do
      name <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- First create the enum type
          Session.statement ()
            $ Statement.preparable
                (mconcat ["create type ", name, " as enum ('sad', 'ok', 'happy')"])
                mempty
                Decoders.noResult
          -- Then test encoding
          Session.statement "ok"
            $ Statement.preparable
                (mconcat ["select $1 = ('ok' :: ", name, ")"])
                (Encoders.param (Encoders.nonNullable Encoders.unknown))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        result `shouldBe` Right True
