module Sharing.ByUnit.Encoders.HstoreSpec (spec) where

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
  describe "Hstore Encoders" do
    it "encodes empty hstore" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Enable hstore extension (unprepared), ignore if already exists
          catchError
            ( Session.statement ()
                $ Statement.preparable
                "CREATE EXTENSION IF NOT EXISTS hstore"
                Encoders.noParams
                Decoders.noResult
                  False
            )
            (const (pure ()))
          -- Test encoding empty hstore
          Session.statement
            ([] :: [(Text, Maybe Text)])
            $ Statement.unpreparable
                "select $1::hstore = ''::hstore"
                (Encoders.param (Encoders.nonNullable Encoders.hstore))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        result `shouldBe` Right True

    it "encodes hstore with single key-value pair" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Enable hstore extension (unprepared), ignore if already exists
          catchError
            ( Session.statement ()
                $ Statement.preparable
                "CREATE EXTENSION IF NOT EXISTS hstore"
                Encoders.noParams
                Decoders.noResult
            )
            (const (pure ()))
          -- Test encoding single key-value pair
          Session.statement
            [("key", Just "value")]
            $ Statement.unpreparable
                "select $1::hstore = 'key => value'::hstore"
                (Encoders.param (Encoders.nonNullable Encoders.hstore))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        result `shouldBe` Right True

    it "encodes hstore with multiple key-value pairs" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Enable hstore extension (unprepared), ignore if already exists
          catchError
            ( Session.statement ()
                $ Statement.preparable
                "CREATE EXTENSION IF NOT EXISTS hstore"
                Encoders.noParams
                Decoders.noResult
            )
            (const (pure ()))
          -- Test encoding multiple key-value pairs
          Session.statement
            [("a", Just "1"), ("b", Just "2"), ("c", Just "3")]
            $ Statement.unpreparable
                "select $1::hstore @> 'a => 1'::hstore AND $1::hstore @> 'b => 2'::hstore AND $1::hstore @> 'c => 3'::hstore"
                (Encoders.param (Encoders.nonNullable Encoders.hstore))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        result `shouldBe` Right True

    it "encodes hstore with null values" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Enable hstore extension (unprepared), ignore if already exists
          catchError
            ( Session.statement ()
                $ Statement.preparable
                "CREATE EXTENSION IF NOT EXISTS hstore"
                Encoders.noParams
                Decoders.noResult
            )
            (const (pure ()))
          -- Test encoding hstore with null values
          Session.statement
            [("key1", Just "value1"), ("key2", Nothing), ("key3", Just "value3")]
            $ Statement.unpreparable
                "select $1::hstore = 'key1 => value1, key2 => NULL, key3 => value3'::hstore"
                (Encoders.param (Encoders.nonNullable Encoders.hstore))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        result `shouldBe` Right True

    it "roundtrips hstore correctly" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let testData = HashMap.fromList [("key1", Just "value1"), ("key2", Nothing), ("key3", Just "value3")]
        result <- Connection.use connection do
          -- Enable hstore extension (unprepared), ignore if already exists
          catchError
            ( Session.statement ()
                $ Statement.preparable
                "CREATE EXTENSION IF NOT EXISTS hstore"
                Encoders.noParams
                Decoders.noResult
            )
            (const (pure ()))
          -- Test roundtrip
          Session.statement
            (HashMap.toList testData)
            $ Statement.unpreparable
                "select $1"
                (Encoders.param (Encoders.nonNullable Encoders.hstore))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.hstore (\n f -> replicateM n f >>= pure . HashMap.fromList)))))
        result `shouldBe` Right testData

    it "encodes hstore with special characters" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Enable hstore extension (unprepared), ignore if already exists
          catchError
            ( Session.statement ()
                $ Statement.preparable
                "CREATE EXTENSION IF NOT EXISTS hstore"
                Encoders.noParams
                Decoders.noResult
            )
            (const (pure ()))
          -- Test encoding hstore with special characters
          Session.statement
            [("key with spaces", Just "value with quotes")]
            $ Statement.Statement
              "select $1::hstore = '\"key with spaces\" => \"value with quotes\"'::hstore"
              (Encoders.param (Encoders.nonNullable Encoders.hstore))
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        result `shouldBe` Right True
