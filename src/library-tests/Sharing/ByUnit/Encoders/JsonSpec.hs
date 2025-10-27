module Sharing.ByUnit.Encoders.JsonSpec (spec) where

import Data.Aeson qualified as Aeson
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
  describe "JSON Encoders" do
    it "encodes JSON object correctly" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.preparable
                "select $1::json"
                (Encoders.param (Encoders.nonNullable Encoders.json))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.json)))
            testValue = Aeson.object [("key", Aeson.String "value")]
        result <- Connection.use connection (Session.statement testValue statement)
        result `shouldBe` Right testValue

    it "roundtrips JSON array" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.preparable
                "select $1"
                (Encoders.param (Encoders.nonNullable Encoders.json))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.json)))
            testValue = Aeson.Array (fromList [Aeson.Number 1, Aeson.Number 2])
        result <- Connection.use connection (Session.statement testValue statement)
        result `shouldBe` Right testValue

  describe "JSONB Encoders" do
    it "encodes JSONB object correctly" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.preparable
                "select $1::jsonb"
                (Encoders.param (Encoders.nonNullable Encoders.jsonb))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.jsonb)))
            testValue = Aeson.object [("name", Aeson.String "test"), ("value", Aeson.Number 123)]
        result <- Connection.use connection (Session.statement testValue statement)
        result `shouldBe` Right testValue

    it "roundtrips JSONB with nested structure" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.preparable
                "select $1"
                (Encoders.param (Encoders.nonNullable Encoders.jsonb))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.jsonb)))
            testValue =
              Aeson.object
                [ ("array", Aeson.Array (fromList [Aeson.Number 1, Aeson.Number 2])),
                  ("nested", Aeson.object [("inner", Aeson.String "value")])
                ]
        result <- Connection.use connection (Session.statement testValue statement)
        result `shouldBe` Right testValue
