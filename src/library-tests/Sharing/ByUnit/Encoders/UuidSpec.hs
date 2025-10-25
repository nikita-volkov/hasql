module Sharing.ByUnit.Encoders.UuidSpec (spec) where

import Data.UUID qualified as UUID
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
  describe "UUID Encoders" do
    it "encodes UUID correctly" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select $1 = '550e8400-e29b-41d4-a716-446655440000'::uuid"
                (Encoders.param (Encoders.nonNullable Encoders.uuid))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
                True
        case UUID.fromString "550e8400-e29b-41d4-a716-446655440000" of
          Just testUuid -> do
            result <- Connection.use connection (Session.statement testUuid statement)
            result `shouldBe` Right True
          Nothing -> expectationFailure "Failed to parse test UUID"

    it "roundtrips UUID correctly" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select $1"
                (Encoders.param (Encoders.nonNullable Encoders.uuid))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.uuid)))
                True
        case UUID.fromString "a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11" of
          Just testUuid -> do
            result <- Connection.use connection (Session.statement testUuid statement)
            result `shouldBe` Right testUuid
          Nothing -> expectationFailure "Failed to parse test UUID"

    it "encodes nil UUID correctly" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select $1"
                (Encoders.param (Encoders.nonNullable Encoders.uuid))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.uuid)))
                True
        result <- Connection.use connection (Session.statement UUID.nil statement)
        result `shouldBe` Right UUID.nil
