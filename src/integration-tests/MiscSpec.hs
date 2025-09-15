module MiscSpec (spec) where

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
  describe "Miscellaneous Tests" do
    describe "Interval types" do
      it "encodes intervals correctly" \connection -> do
        let statement =
              Statement.Statement
                "select $1 = interval '10 seconds'"
                (Encoders.param (Encoders.nonNullable Encoders.interval))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
                True
        result <- Connection.use connection (Session.statement (10 :: DiffTime) statement)
        result `shouldBe` Right True

      it "decodes intervals correctly" \connection -> do
        let statement =
              Statement.Statement
                "select interval '10 seconds'"
                Encoders.noParams
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.interval)))
                True
        result <- Connection.use connection (Session.statement () statement)
        result `shouldBe` Right (10 :: DiffTime)

      it "roundtrips intervals correctly" \connection -> do
        let statement =
              Statement.Statement
                "select $1"
                (Encoders.param (Encoders.nonNullable Encoders.interval))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.interval)))
                True
        result <- Connection.use connection (Session.statement (10 :: DiffTime) statement)
        result `shouldBe` Right (10 :: DiffTime)

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

    describe "Transaction-like operations" do
      it "handles in progress after error scenario" \connection -> do
        let sumStatement =
              Statement.Statement
                "select ($1 + $2)"
                ( contramap fst (Encoders.param (Encoders.nonNullable Encoders.int8))
                    <> contramap snd (Encoders.param (Encoders.nonNullable Encoders.int8))
                )
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
                True

        result <-
          Connection.use connection do
            Session.sql "begin;"
            s <- Session.statement (1 :: Int64, 1 :: Int64) sumStatement
            Session.sql "end;"
            return s
        result `shouldBe` Right (2 :: Int64)

      it "recovers properly after query errors" \connection -> do
        let tryStatement =
              Statement.Statement
                "select $1 :: int8"
                (Encoders.param (Encoders.nonNullable Encoders.int8))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
                True

        result <-
          Connection.use connection do
            -- First successful query
            _ <- Session.statement (1 :: Int64) tryStatement
            -- This should fail but connection should remain usable
            _ <- catchError (Session.sql "absurd") (const (pure ()))
            -- Second successful query
            Session.statement (1 :: Int64) tryStatement

        result `shouldSatisfy` isRight
  where
    isRight (Right _) = True
    isRight _ = False
