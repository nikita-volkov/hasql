module Isolated.Features.CodecsSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec
import TestingKit.Testcontainers qualified as Testcontainers
import Prelude

spec :: Spec
spec = Testcontainers.aroundSpecWithConnection False do
  describe "Enum and Custom Type Tests" do
    describe "Unknown types" do
      it "handles unknown type encoding" \connection -> do
        -- First create the enum type
        let dropStatement = Statement.Statement "drop type if exists mood" mempty Decoders.noResult True
        let createStatement = Statement.Statement "create type mood as enum ('sad', 'ok', 'happy')" mempty Decoders.noResult True
        let testStatement =
              Statement.Statement
                "select $1 = ('ok' :: mood)"
                (Encoders.param (Encoders.nonNullable Encoders.unknown))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
                True

        result <-
          Connection.use connection do
            Session.statement () dropStatement
            Session.statement () createStatement
            Session.statement "ok" testStatement
        result `shouldBe` Right True

      it "handles enum encoding and decoding" \connection -> do
        -- First create the enum type
        let dropStatement = Statement.Statement "drop type if exists mood" mempty Decoders.noResult True
        let createStatement = Statement.Statement "create type mood as enum ('sad', 'ok', 'happy')" mempty Decoders.noResult True
        let testStatement =
              Statement.Statement
                "select ($1 :: mood)"
                (Encoders.param (Encoders.nonNullable (Encoders.enum id)))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.enum (Just . id)))))
                True

        result <-
          Connection.use connection do
            Session.statement () dropStatement
            Session.statement () createStatement
            Session.statement "ok" testStatement
        result `shouldBe` Right "ok"

    describe "Unknown enum" do
      it "handles unknown enum encoding" \connection -> do
        -- First create the enum type
        let dropStatement = Statement.Statement "drop type if exists mood" mempty Decoders.noResult True
        let createStatement = Statement.Statement "create type mood as enum ('sad', 'ok', 'happy')" mempty Decoders.noResult True
        let testStatement =
              Statement.Statement
                "select $1"
                (Encoders.param (Encoders.nonNullable (Encoders.unknownEnum id)))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.enum (Just . id)))))
                True

        result <-
          Connection.use connection do
            Session.statement () dropStatement
            Session.statement () createStatement
            Session.statement "ok" testStatement
        result `shouldBe` Right "ok"
