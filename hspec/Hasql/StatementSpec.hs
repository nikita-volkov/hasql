module Hasql.StatementSpec (spec) where

import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Hasql.TestingKit.Testcontainers qualified as Testcontainers
import Test.Hspec
import Prelude

spec :: Spec
spec = aroundAll Testcontainers.withConnection do
  describe "Statement Functionality" do
    describe "Prepared statements" do
      it "allows reuse of the same prepared statement on different types" \connection -> do
        let statement1 =
              Statement.Statement
                "select $1"
                (Encoders.param (Encoders.nonNullable Encoders.text))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
                True
        let statement2 =
              Statement.Statement
                "select $1"
                (Encoders.param (Encoders.nonNullable Encoders.int8))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
                True

        result <-
          Session.run
            ( do
                result1 <- Session.statement "ok" statement1
                result2 <- Session.statement (1 :: Int64) statement2
                return (result1, result2)
            )
            connection
        result `shouldBe` Right ("ok", 1 :: Int64)

    describe "Row counting" do
      it "counts affected rows correctly" \connection -> do
        let dropTable = Statement.Statement "drop table if exists a" mempty Decoders.noResult True
        let createTable = Statement.Statement "create table a (id bigserial not null, name varchar not null, primary key (id))" mempty Decoders.noResult True
        let insertRow = Statement.Statement "insert into a (name) values ('a')" mempty Decoders.noResult False
        let deleteRows = Statement.Statement "delete from a" mempty Decoders.rowsAffected False

        result <-
          Session.run
            ( do
                Session.statement () dropTable
                Session.statement () createTable
                replicateM_ 100 (Session.statement () insertRow)
                affectedRows <- Session.statement () deleteRows
                Session.statement () dropTable
                return affectedRows
            )
            connection
        result `shouldBe` Right 100

    describe "Auto-incremented columns" do
      it "returns auto-incremented column results" \connection -> do
        let dropTable = Statement.Statement "drop table if exists a" mempty Decoders.noResult True
        let createTable = Statement.Statement "create table a (id bigserial not null, name varchar not null, primary key (id))" mempty Decoders.noResult True
        let insertRow = Statement.Statement "insert into a (name) values ('a') returning id" mempty (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))) False
        let insertRow2 = Statement.Statement "insert into a (name) values ('b') returning id" mempty (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))) False

        result <-
          Session.run
            ( do
                Session.statement () dropTable
                Session.statement () createTable
                id1 <- Session.statement () insertRow
                id2 <- Session.statement () insertRow2
                Session.statement () dropTable
                return (id1, id2)
            )
            connection
        result `shouldBe` Right (1 :: Int64, 2 :: Int64)

    describe "List decoding" do
      it "decodes lists correctly" \connection -> do
        let statement =
              Statement.Statement
                "values (1,2), (3,4), (5,6)"
                mempty
                (Decoders.rowList ((,) <$> (Decoders.column (Decoders.nonNullable Decoders.int8)) <*> (Decoders.column (Decoders.nonNullable Decoders.int8))))
                True
        result <- Session.run (Session.statement () statement) connection
        result `shouldBe` Right [(1 :: Int64, 2 :: Int64), (3, 4), (5, 6)]
