module Sharing.ByUnit.Encoders.UnnamedEnumSpec (spec) where

import Data.Text.Encoding (encodeUtf8)
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
  describe "Unnamed Enum Encoders" do
    describe "Simple enums" do
      it "encodes a simple unnamed enum with explicit type cast" \config -> do
        enumName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create enum type
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", enumName, " as enum ('sad', 'ok', 'happy')"]))
                mempty
                Decoders.noResult
                True
            -- Test encoding with explicit cast in SQL
            Session.statement "ok"
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select ($1 :: ", enumName, ") = 'ok' :: ", enumName]))
                (Encoders.param (Encoders.nonNullable (Encoders.unnamedEnum id)))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
                True
          result `shouldBe` Right True

      it "roundtrips a simple unnamed enum" \config -> do
        enumName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create enum type
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", enumName, " as enum ('alpha', 'beta', 'gamma')"]))
                mempty
                Decoders.noResult
                True
            -- Test roundtrip with explicit cast
            Session.statement "gamma"
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select $1 :: ", enumName]))
                (Encoders.param (Encoders.nonNullable (Encoders.unnamedEnum id)))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.namedEnum Nothing enumName (Just . id)))))
                True
          result `shouldBe` Right "gamma"
