module Sharing.ByUnit.StatementSpec (spec) where

import Data.Text.Encoding (encodeUtf8)
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic (assert, monadicIO, pre, run)
import Prelude hiding (assert)

spec :: SpecWith (Text, Word16)
spec = do
  describe "Codecs" do
    describe "Interval types" do
      it "encodes intervals correctly" \config -> do
        Scripts.onConnection config \connection -> do
          let statement =
                Statement.Statement
                  "select $1 = interval '10 seconds'"
                  (Encoders.param (Encoders.nonNullable Encoders.interval))
                  (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
                  True
          result <- Connection.use connection (Session.statement (10 :: DiffTime) statement)
          result `shouldBe` Right True

      it "decodes intervals correctly" \config -> do
        Scripts.onConnection config \connection -> do
          let statement =
                Statement.Statement
                  "select interval '10 seconds'"
                  Encoders.noParams
                  (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.interval)))
                  True
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right (10 :: DiffTime)

      it "roundtrips intervals correctly" \config -> do
        Scripts.onConnection config \connection -> do
          let statement =
                Statement.Statement
                  "select $1"
                  (Encoders.param (Encoders.nonNullable Encoders.interval))
                  (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.interval)))
                  True
          result <- Connection.use connection (Session.statement (10 :: DiffTime) statement)
          result `shouldBe` Right (10 :: DiffTime)

    describe "Unknown types" do
      it "handles unknown type encoding" \config -> do
        name <- Scripts.generateSymname
        Scripts.onConnection config \connection -> do
          result <- Connection.use connection do
            -- First create the enum type
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", name, " as enum ('sad', 'ok', 'happy')"]))
                mempty
                Decoders.noResult
                True
            -- Then test encoding
            Session.statement "ok"
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select $1 = ('ok' :: ", name, ")"]))
                (Encoders.param (Encoders.nonNullable Encoders.unknown))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
                True
          result `shouldBe` Right True

    describe "Array roundtrips" do
      it "handles 1D arrays" \config -> property $ \(values :: [Int64]) -> monadicIO $ do
        let statement =
              Statement.Statement
                "select $1"
                (Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable Encoders.int8))))))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.array (Decoders.dimension replicateM (Decoders.element (Decoders.nonNullable Decoders.int8)))))))
                True
        result <- run $ Scripts.onConnection config \connection ->
          Connection.use connection (Session.statement values statement)
        assert $ result == Right values

      it "handles 2D arrays" \config -> property $ \(values :: [Int64]) -> monadicIO $ do
        pre (not (null values))
        let input = replicate 3 values
        let statement =
              Statement.Statement
                "select $1"
                (Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable Encoders.int8)))))))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.array (Decoders.dimension replicateM (Decoders.dimension replicateM (Decoders.element (Decoders.nonNullable Decoders.int8))))))))
                True
        result <- run $ Scripts.onConnection config \connection ->
          Connection.use connection (Session.statement input statement)
        assert $ result == Right input

    describe "Composite types" do
      it "decodes simple composites" \config -> do
        Scripts.onConnection config \connection -> do
          let statement =
                Statement.Statement
                  "select (1, true)"
                  mempty
                  (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.composite ((,) <$> (Decoders.field (Decoders.nonNullable Decoders.int8)) <*> (Decoders.field (Decoders.nonNullable Decoders.bool)))))))
                  True
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right (1 :: Int64, True)

      it "decodes complex composites" \config -> do
        Scripts.onConnection config \connection -> do
          let statement =
                Statement.Statement
                  "select ((1, true), ('hello', 3))"
                  mempty
                  ( Decoders.singleRow
                      ( Decoders.column
                          ( Decoders.nonNullable
                              ( Decoders.composite
                                  ( (,)
                                      <$> ( Decoders.field
                                              ( Decoders.nonNullable
                                                  ( Decoders.composite
                                                      ( (,)
                                                          <$> (Decoders.field (Decoders.nonNullable Decoders.int8))
                                                          <*> (Decoders.field (Decoders.nonNullable Decoders.bool))
                                                      )
                                                  )
                                              )
                                          )
                                      <*> ( Decoders.field
                                              ( Decoders.nonNullable
                                                  ( Decoders.composite
                                                      ( (,)
                                                          <$> (Decoders.field (Decoders.nonNullable Decoders.text))
                                                          <*> (Decoders.field (Decoders.nonNullable Decoders.int8))
                                                      )
                                                  )
                                              )
                                          )
                                  )
                              )
                          )
                      )
                  )
                  True
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right ((1 :: Int64, True), ("hello", 3 :: Int64))

    describe "Enum types" do
      it "handles enum encoding and decoding" \config -> do
        name <- Scripts.generateSymname
        Scripts.onConnection config \connection -> do
          result <- Connection.use connection do
            -- First create the enum type
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", name, " as enum ('sad', 'ok', 'happy')"]))
                mempty
                Decoders.noResult
                True
            -- Then test encoding and decoding
            Session.statement "ok"
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select ($1 :: ", name, ")"]))
                (Encoders.param (Encoders.nonNullable (Encoders.enum id)))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.enum (Just . id)))))
                True
          result `shouldBe` Right "ok"

    describe "Unknown enum" do
      it "handles unknown enum encoding" \config -> do
        name <- Scripts.generateSymname
        Scripts.onConnection config \connection -> do
          result <- Connection.use connection do
            -- First create the enum type
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", name, " as enum ('sad', 'ok', 'happy')"]))
                mempty
                Decoders.noResult
                True
            -- Then test encoding
            Session.statement "ok"
              $ Statement.Statement
                "select $1"
                (Encoders.param (Encoders.nonNullable (Encoders.unknownEnum id)))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.enum (Just . id)))))
                True
          result `shouldBe` Right "ok"

  describe "Statement Functionality" do
    describe "Prepared statements" do
      it "allows reuse of the same prepared statement on different types" \config -> do
        Scripts.onConnection config \connection -> do
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
        Scripts.onConnection config \connection -> do
          let dropTable = Statement.Statement (encodeUtf8 ("drop table if exists " <> tableName)) mempty Decoders.noResult True
          let createTable = Statement.Statement (encodeUtf8 ("create table " <> tableName <> " (id bigserial not null, name varchar not null, primary key (id))")) mempty Decoders.noResult True
          let insertRow = Statement.Statement (encodeUtf8 ("insert into " <> tableName <> " (name) values ('a')")) mempty Decoders.noResult False
          let deleteRows = Statement.Statement (encodeUtf8 ("delete from " <> tableName)) mempty Decoders.rowsAffected False

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
        Scripts.onConnection config \connection -> do
          let dropTable = Statement.Statement (encodeUtf8 ("drop table if exists " <> tableName)) mempty Decoders.noResult True
          let createTable = Statement.Statement (encodeUtf8 ("create table " <> tableName <> " (id bigserial not null, name varchar not null, primary key (id))")) mempty Decoders.noResult True
          let insertRow = Statement.Statement (encodeUtf8 ("insert into " <> tableName <> " (name) values ('a') returning id")) mempty (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))) False
          let insertRow2 = Statement.Statement (encodeUtf8 ("insert into " <> tableName <> " (name) values ('b') returning id")) mempty (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))) False

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
        Scripts.onConnection config \connection -> do
          let statement =
                Statement.Statement
                  "values (1 :: int8, 2 :: int8), (3,4), (5,6)"
                  mempty
                  (Decoders.rowList ((,) <$> (Decoders.column (Decoders.nonNullable Decoders.int8)) <*> (Decoders.column (Decoders.nonNullable Decoders.int8))))
                  True
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right [(1 :: Int64, 2 :: Int64), (3, 4), (5, 6)]
