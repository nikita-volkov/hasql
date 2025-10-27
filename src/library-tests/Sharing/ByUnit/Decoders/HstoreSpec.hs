module Sharing.ByUnit.Decoders.HstoreSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
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
  describe "Hstore Decoders" do
    it "decodes empty hstore" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Enable hstore extension (unprepared), ignore if already exists
          catchError
            ( Session.statement ()
                $ Statement.unpreparable
                  "CREATE EXTENSION IF NOT EXISTS hstore"
                  Encoders.noParams
                  Decoders.noResult
            )
            (const (pure ()))
          -- Test decoding empty hstore
          Session.statement ()
            $ Statement.preparable
              "select ''::hstore"
              Encoders.noParams
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.hstore (\n f -> replicateM n f >>= pure . HashMap.fromList)))))
        result `shouldBe` Right (HashMap.empty :: HashMap.HashMap Text (Maybe Text))

    it "decodes hstore with single key-value pair" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Enable hstore extension (unprepared), ignore if already exists
          catchError
            ( Session.statement ()
                $ Statement.unpreparable
                  "CREATE EXTENSION IF NOT EXISTS hstore"
                  Encoders.noParams
                  Decoders.noResult
            )
            (const (pure ()))
          -- Test decoding single key-value pair
          Session.statement ()
            $ Statement.preparable
              "select 'key => value'::hstore"
              Encoders.noParams
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.hstore (\n f -> replicateM n f >>= pure . HashMap.fromList)))))
        result `shouldBe` Right (HashMap.fromList [("key", Just "value")] :: HashMap.HashMap Text (Maybe Text))

    it "decodes hstore with multiple key-value pairs" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Enable hstore extension (unprepared), ignore if already exists
          catchError
            ( Session.statement ()
                $ Statement.unpreparable
                  "CREATE EXTENSION IF NOT EXISTS hstore"
                  Encoders.noParams
                  Decoders.noResult
            )
            (const (pure ()))
          -- Test decoding multiple key-value pairs
          Session.statement ()
            $ Statement.preparable
              "select 'a => 1, b => 2, c => 3'::hstore"
              Encoders.noParams
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.hstore (\n f -> replicateM n f >>= pure . HashMap.fromList)))))
        result `shouldBe` Right (HashMap.fromList [("a", Just "1"), ("b", Just "2"), ("c", Just "3")] :: HashMap.HashMap Text (Maybe Text))

    it "decodes hstore with null values" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Enable hstore extension (unprepared), ignore if already exists
          catchError
            ( Session.statement ()
                $ Statement.unpreparable
                  "CREATE EXTENSION IF NOT EXISTS hstore"
                  Encoders.noParams
                  Decoders.noResult
            )
            (const (pure ()))
          -- Test decoding hstore with null values
          Session.statement ()
            $ Statement.preparable
              "select 'key1 => value1, key2 => NULL, key3 => value3'::hstore"
              Encoders.noParams
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.hstore (\n f -> replicateM n f >>= pure . HashMap.fromList)))))
        result `shouldBe` Right (HashMap.fromList [("key1", Just "value1"), ("key2", Nothing), ("key3", Just "value3")] :: HashMap.HashMap Text (Maybe Text))

    it "decodes hstore with special characters" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Enable hstore extension (unprepared), ignore if already exists
          catchError
            ( Session.statement ()
                $ Statement.unpreparable
                  "CREATE EXTENSION IF NOT EXISTS hstore"
                  Encoders.noParams
                  Decoders.noResult
            )
            (const (pure ()))
          -- Test decoding hstore with special characters
          Session.statement ()
            $ Statement.preparable
              "select '\"key with spaces\" => \"value with quotes\"'::hstore"
              Encoders.noParams
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.hstore (\n f -> replicateM n f >>= pure . HashMap.fromList)))))
        result `shouldBe` Right (HashMap.fromList [("key with spaces", Just "value with quotes")] :: HashMap.HashMap Text (Maybe Text))
