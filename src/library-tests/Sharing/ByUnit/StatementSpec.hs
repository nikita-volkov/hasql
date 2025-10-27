module Sharing.ByUnit.StatementSpec (spec) where

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
  describe "Statement Functionality" do
    describe "Prepared statements" do
      it "allows reuse of the same prepared statement on different types" \config -> do
        Scripts.onPreparableConnection config \connection -> do
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
            Connection.use connection do
              result1 <- Session.statement "ok" statement1
              result2 <- Session.statement (1 :: Int64) statement2
              return (result1, result2)
          result `shouldBe` Right ("ok", 1 :: Int64)

    describe "Row counting" do
      it "counts affected rows correctly" \config -> do
        tableName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          let dropTable = Statement.Statement ("drop table if exists " <> tableName) mempty Decoders.noResult True
          let createTable = Statement.Statement ("create table " <> tableName <> " (id bigserial not null, name varchar not null, primary key (id))") mempty Decoders.noResult True
          let insertRow = Statement.Statement ("insert into " <> tableName <> " (name) values ('a')") mempty Decoders.noResult False
          let deleteRows = Statement.Statement ("delete from " <> tableName) mempty Decoders.rowsAffected False

          result <-
            Connection.use connection do
              Session.statement () dropTable
              Session.statement () createTable
              replicateM_ 100 (Session.statement () insertRow)
              affectedRows <- Session.statement () deleteRows
              Session.statement () dropTable
              return affectedRows
          result `shouldBe` Right 100

    describe "Auto-incremented columns" do
      it "returns auto-incremented column results" \config -> do
        tableName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          let dropTable = Statement.Statement ("drop table if exists " <> tableName) mempty Decoders.noResult True
          let createTable = Statement.Statement ("create table " <> tableName <> " (id bigserial not null, name varchar not null, primary key (id))") mempty Decoders.noResult True
          let insertRow = Statement.Statement ("insert into " <> tableName <> " (name) values ('a') returning id") mempty (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))) False
          let insertRow2 = Statement.Statement ("insert into " <> tableName <> " (name) values ('b') returning id") mempty (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))) False

          result <-
            Connection.use connection do
              Session.statement () dropTable
              Session.statement () createTable
              id1 <- Session.statement () insertRow
              id2 <- Session.statement () insertRow2
              Session.statement () dropTable
              return (id1, id2)
          result `shouldBe` Right (1 :: Int64, 2 :: Int64)

    describe "List decoding" do
      it "decodes lists correctly" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.Statement
                  "values (1 :: int8, 2 :: int8), (3,4), (5,6)"
                  mempty
                  (Decoders.rowList ((,) <$> (Decoders.column (Decoders.nonNullable Decoders.int8)) <*> (Decoders.column (Decoders.nonNullable Decoders.int8))))
                  True
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right [(1 :: Int64, 2 :: Int64), (3, 4), (5, 6)]

    describe "IN simulation" do
      it "works with arrays" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.Statement
                  "select true where 1 = any ($1)"
                  (Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable Encoders.int8))))))
                  (fmap (maybe False (const True)) (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.bool))))
                  True
          result <- Connection.use connection do
            result1 <- Session.statement ([1, 2] :: [Int64]) statement
            result2 <- Session.statement ([2, 3] :: [Int64]) statement
            return (result1, result2)
          result `shouldBe` Right (True, False)

    describe "NOT IN simulation" do
      it "works with arrays" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.Statement
                  "select true where 3 <> all ($1)"
                  (Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable Encoders.int8))))))
                  (fmap (maybe False (const True)) (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.bool))))
                  True
          result <- Connection.use connection do
            result1 <- Session.statement ([1, 2] :: [Int64]) statement
            result2 <- Session.statement ([2, 3] :: [Int64]) statement
            return (result1, result2)
          result `shouldBe` Right (True, False)
