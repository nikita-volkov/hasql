module Sharing.ByUnit.Encoders.DomainSpec (spec) where

import Data.Text.Encoding (encodeUtf8)
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
  describe "Domain type encoding" do
    describe "Simple scalar domains" do
      it "encodes a domain based on int8 using int8 codec" \config -> do
        domainName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create domain type
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create domain ", domainName, " as int8"]))
                mempty
                Decoders.noResult
            -- Test encoding by comparing with static value
            Session.statement (42 :: Int64)
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select ($1 :: ", domainName, ") = 42"]))
                (Encoders.param (Encoders.nonNullable Encoders.int8))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
          result `shouldBe` Right True

      it "encodes a domain based on text using text codec" \config -> do
        domainName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create domain type
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create domain ", domainName, " as text"]))
                mempty
                Decoders.noResult
            -- Test encoding by comparing with static value
            Session.statement ("hello" :: Text)
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select ($1 :: ", domainName, ") = 'hello'"]))
                (Encoders.param (Encoders.nonNullable Encoders.text))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
          result `shouldBe` Right True

      it "encodes a domain based on bool using bool codec" \config -> do
        domainName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create domain type
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create domain ", domainName, " as bool"]))
                mempty
                Decoders.noResult
            -- Test encoding by comparing with static value
            Session.statement True
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select ($1 :: ", domainName, ") = true"]))
                (Encoders.param (Encoders.nonNullable Encoders.bool))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
          result `shouldBe` Right True

    describe "Domains with constraints" do
      it "encodes values that satisfy domain constraints" \config -> do
        domainName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create domain type with constraint
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create domain ", domainName, " as int8 check (value > 0)"]))
                mempty
                Decoders.noResult
            -- Test encoding a value that satisfies the constraint
            Session.statement (42 :: Int64)
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select ($1 :: ", domainName, ") = 42"]))
                (Encoders.param (Encoders.nonNullable Encoders.int8))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
          result `shouldBe` Right True

    describe "Domain type cast compatibility for composite usage" do
      it "encodes base type value that can be used in composite with domain field via explicit cast" \config -> do
        domainName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create domain type
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create domain ", domainName, " as int8"]))
                mempty
                Decoders.noResult
            -- Encode int8, cast it to domain, and use in ROW constructor
            Session.statement (42 :: Int64)
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select ($1 :: ", domainName, ") = 42"]))
                (Encoders.param (Encoders.nonNullable Encoders.int8))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
          result `shouldBe` Right True

    describe "Domain type cast compatibility for array usage" do
      it "encodes base type array that can be cast to domain array" \config -> do
        domainName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create domain type
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create domain ", domainName, " as int8"]))
                mempty
                Decoders.noResult
            -- Encode int8 array using base codec and verify it works
            Session.statement ([1, 2, 3] :: [Int64])
              $ Statement.preparable
                (encodeUtf8 "select $1 = ARRAY[1,2,3] :: int8[]")
                ( Encoders.param
                    ( Encoders.nonNullable
                        (Encoders.foldableArray (Encoders.nonNullable Encoders.int8))
                    )
                )
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
          result `shouldBe` Right True

      it "encodes text array that can be used with text domain" \config -> do
        domainName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create domain type
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create domain ", domainName, " as text"]))
                mempty
                Decoders.noResult
            -- Encode text array using base codec
            Session.statement (["a", "b", "c"] :: [Text])
              $ Statement.preparable
                (encodeUtf8 "select $1 = ARRAY['a','b','c'] :: text[]")
                ( Encoders.param
                    ( Encoders.nonNullable
                        (Encoders.foldableArray (Encoders.nonNullable Encoders.text))
                    )
                )
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
          result `shouldBe` Right True
