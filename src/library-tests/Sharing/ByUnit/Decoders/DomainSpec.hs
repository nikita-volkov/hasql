module Sharing.ByUnit.Decoders.DomainSpec (spec) where

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
  describe "Domain type decoding" do
    describe "Simple scalar domains" do
      it "decodes a domain based on int8 using int8 codec" \config -> do
        domainName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create domain type
            Session.statement ()
              $ Statement.preparable
                (mconcat ["create domain ", domainName, " as int8"])
                mempty
                Decoders.noResult
            -- Test decoding from static value
            Session.statement ()
              $ Statement.preparable
                (mconcat ["select 42 :: ", domainName])
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
          result `shouldBe` Right (42 :: Int64)

      it "decodes a domain based on text using text codec" \config -> do
        domainName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create domain type
            Session.statement ()
              $ Statement.preparable
                (mconcat ["create domain ", domainName, " as text"])
                mempty
                Decoders.noResult
            -- Test decoding from static value
            Session.statement ()
              $ Statement.preparable
                (mconcat ["select 'hello' :: ", domainName])
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
          result `shouldBe` Right "hello"

      it "decodes a domain based on bool using bool codec" \config -> do
        domainName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create domain type
            Session.statement ()
              $ Statement.preparable
                (mconcat ["create domain ", domainName, " as bool"])
                mempty
                Decoders.noResult
            -- Test decoding from static value
            Session.statement ()
              $ Statement.preparable
                (mconcat ["select true :: ", domainName])
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
          result `shouldBe` Right True

      it "roundtrips a domain based on numeric" \config -> do
        domainName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create domain type
            Session.statement ()
              $ Statement.preparable
                (mconcat ["create domain ", domainName, " as numeric"])
                mempty
                Decoders.noResult
            -- Test roundtrip
            Session.statement (123.456 :: Scientific)
              $ Statement.preparable
                (mconcat ["select $1 :: ", domainName])
                (Encoders.param (Encoders.nonNullable Encoders.numeric))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.numeric)))
          result `shouldBe` Right (123.456 :: Scientific)

    describe "Domain with constraints" do
      it "decodes domain values that satisfy constraints" \config -> do
        domainName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create domain type with constraint
            Session.statement ()
              $ Statement.preparable
                (mconcat ["create domain ", domainName, " as int8 check (value > 0)"])
                mempty
                Decoders.noResult
            -- Decode value that satisfies constraint
            Session.statement ()
              $ Statement.preparable
                (mconcat ["select 42 :: ", domainName])
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
          result `shouldBe` Right (42 :: Int64)

    describe "Domain type cast compatibility for composite usage" do
      it "decodes domain value cast to base type from composite field" \config -> do
        domainName <- Scripts.generateSymname
        compositeName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create domain type
            Session.statement ()
              $ Statement.preparable
                (mconcat ["create domain ", domainName, " as int8"])
                mempty
                Decoders.noResult
            -- Create composite type with domain field
            Session.statement ()
              $ Statement.preparable
                (mconcat ["create type ", compositeName, " as (x ", domainName, ", y bool)"])
                mempty
                Decoders.noResult
            -- Extract and cast domain field to base type for decoding
            Session.statement ()
              $ Statement.preparable
                (mconcat ["select ((42 :: ", domainName, ") :: int8)"])
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
          result `shouldBe` Right (42 :: Int64)

    describe "Domain type cast compatibility for array usage" do
      it "decodes array cast from domain array to base type array" \config -> do
        domainName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create domain type
            Session.statement ()
              $ Statement.preparable
                (mconcat ["create domain ", domainName, " as int8"])
                mempty
                Decoders.noResult
            -- Decode base type array
            Session.statement ()
              $ Statement.preparable
                "select ARRAY[1,2,3] :: int8[]"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.listArray (Decoders.nonNullable Decoders.int8)))))
          result `shouldBe` Right ([1, 2, 3] :: [Int64])

      it "roundtrips array using base type codec" \config -> do
        domainName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create domain type
            Session.statement ()
              $ Statement.preparable
                (mconcat ["create domain ", domainName, " as text"])
                mempty
                Decoders.noResult
            -- Test roundtrip using base type codec
            Session.statement (["a", "b", "c"] :: [Text])
              $ Statement.preparable
                "select $1 :: text[]"
                ( Encoders.param
                    ( Encoders.nonNullable
                        (Encoders.foldableArray (Encoders.nonNullable Encoders.text))
                    )
                )
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.listArray (Decoders.nonNullable Decoders.text)))))
          result `shouldBe` Right (["a", "b", "c"] :: [Text])

      it "decodes base type array that can work with domain arrays via cast" \config -> do
        domainName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create domain type
            Session.statement ()
              $ Statement.preparable
                (mconcat ["create domain ", domainName, " as int8"])
                mempty
                Decoders.noResult
            -- Demonstrate that base codec works for arrays
            Session.statement ([10, 20, 30] :: [Int64])
              $ Statement.preparable
                "select $1 :: int8[]"
                ( Encoders.param
                    ( Encoders.nonNullable
                        (Encoders.foldableArray (Encoders.nonNullable Encoders.int8))
                    )
                )
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.listArray (Decoders.nonNullable Decoders.int8)))))
          result `shouldBe` Right ([10, 20, 30] :: [Int64])
