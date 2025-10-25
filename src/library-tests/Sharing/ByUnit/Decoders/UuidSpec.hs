module Sharing.ByUnit.Decoders.UuidSpec (spec) where

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
  describe "UUID Decoders" do
    it "decodes UUID from static value" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select '550e8400-e29b-41d4-a716-446655440000'::uuid"
                Encoders.noParams
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.uuid)))
                True
        result <- Connection.use connection (Session.statement () statement)
        case UUID.fromString "550e8400-e29b-41d4-a716-446655440000" of
          Just expectedUuid -> result `shouldBe` Right expectedUuid
          Nothing -> expectationFailure "Failed to parse expected UUID"

    it "decodes nil UUID" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select '00000000-0000-0000-0000-000000000000'::uuid"
                Encoders.noParams
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.uuid)))
                True
        result <- Connection.use connection (Session.statement () statement)
        result `shouldBe` Right UUID.nil
