module Sharing.ByUnit.Decoders.JsonSpec (spec) where

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
  describe "JSON Decoders" do
    it "decodes JSON null" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select 'null'::json"
                Encoders.noParams
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.json)))
                True
        result <- Connection.use connection (Session.statement () statement)
        result `shouldBe` Right Aeson.Null

    it "decodes JSON number" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select '42'::json"
                Encoders.noParams
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.json)))
                True
        result <- Connection.use connection (Session.statement () statement)
        result `shouldBe` Right (Aeson.Number 42)

    it "decodes JSON string" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select '\"hello\"'::json"
                Encoders.noParams
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.json)))
                True
        result <- Connection.use connection (Session.statement () statement)
        result `shouldBe` Right (Aeson.String "hello")

    it "decodes JSON array" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select '[1,2,3]'::json"
                Encoders.noParams
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.json)))
                True
        result <- Connection.use connection (Session.statement () statement)
        result `shouldBe` Right (Aeson.Array (fromList [Aeson.Number 1, Aeson.Number 2, Aeson.Number 3]))

    it "decodes JSON object" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select '{\"name\":\"John\",\"age\":30}'::json"
                Encoders.noParams
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.json)))
                True
        result <- Connection.use connection (Session.statement () statement)
        result `shouldBe` Right (Aeson.object [("name", Aeson.String "John"), ("age", Aeson.Number 30)])

  describe "JSONB Decoders" do
    it "decodes JSONB object" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select '{\"key\":\"value\"}'::jsonb"
                Encoders.noParams
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.jsonb)))
                True
        result <- Connection.use connection (Session.statement () statement)
        result `shouldBe` Right (Aeson.object [("key", Aeson.String "value")])

    it "decodes JSONB array" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select '[true, false]'::jsonb"
                Encoders.noParams
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.jsonb)))
                True
        result <- Connection.use connection (Session.statement () statement)
        result `shouldBe` Right (Aeson.Array (fromList [Aeson.Bool True, Aeson.Bool False]))
