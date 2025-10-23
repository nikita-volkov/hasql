module Sharing.ByUnit.Encoders.NamedEnumSpec (spec) where

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
  describe "Named Enum Encoders" do
    describe "Simple enums" do
      it "encodes a simple named enum and compares with static value" \config -> do
        enumName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create enum type
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", enumName, " as enum ('sad', 'ok', 'happy')"]))
                mempty
                Decoders.noResult
                True
            -- Test encoding by comparing with static value
            Session.statement "ok"
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select ($1 :: ", enumName, ") = 'ok' :: ", enumName]))
                (Encoders.param (Encoders.nonNullable (Encoders.namedEnum id)))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
                True
          result `shouldBe` Right True

      it "encodes and roundtrips a simple named enum" \config -> do
        enumName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create enum type
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", enumName, " as enum ('sad', 'ok', 'happy')"]))
                mempty
                Decoders.noResult
                True
            -- Test roundtrip
            Session.statement "happy"
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select $1 :: ", enumName]))
                (Encoders.param (Encoders.nonNullable (Encoders.namedEnum id)))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.namedEnum Nothing enumName (Just . id)))))
                True
          result `shouldBe` Right "happy"

    describe "Arrays of enums" do
      it "encodes arrays of named enums" \config -> do
        enumName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create enum type
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", enumName, " as enum ('small', 'medium', 'large')"]))
                mempty
                Decoders.noResult
                True
            -- Test array encoding
            Session.statement ["small", "large", "medium"]
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select ($1 :: ", enumName, "[]) = array['small', 'large', 'medium'] :: ", enumName, "[]"]))
                ( Encoders.param
                    ( Encoders.nonNullable
                        ( Encoders.array
                            ( Encoders.dimension
                                foldl'
                                (Encoders.element (Encoders.nonNullable (Encoders.namedEnum id)))
                            )
                        )
                    )
                )
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
                True
          result `shouldBe` Right True

      it "roundtrips arrays of named enums" \config -> do
        enumName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create enum type
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", enumName, " as enum ('alpha', 'beta', 'gamma')"]))
                mempty
                Decoders.noResult
                True
            -- Test roundtrip
            Session.statement ["beta", "alpha", "gamma"]
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select $1 :: ", enumName, "[]"]))
                ( Encoders.param
                    ( Encoders.nonNullable
                        ( Encoders.array
                            ( Encoders.dimension
                                foldl'
                                (Encoders.element (Encoders.nonNullable (Encoders.namedEnum id)))
                            )
                        )
                    )
                )
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            ( Decoders.array
                                ( Decoders.dimension
                                    replicateM
                                    (Decoders.element (Decoders.nonNullable (Decoders.namedEnum Nothing enumName (Just . id))))
                                )
                            )
                        )
                    )
                )
                True
          result `shouldBe` Right ["beta", "alpha", "gamma"]

    describe "OID lookup verification" do
      it "requests OID for named enums (verified by successful execution)" \config -> do
        -- This test verifies that OID lookup happens by ensuring a named enum
        -- type works correctly - if OID lookup didn't happen, the statement would fail
        enumName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create enum type
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", enumName, " as enum ('first', 'second')"]))
                mempty
                Decoders.noResult
                True
            -- Use named enum - this requires OID lookup to succeed
            Session.statement "second"
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select $1 :: ", enumName]))
                (Encoders.param (Encoders.nonNullable (Encoders.namedEnum id)))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.namedEnum Nothing enumName (Just . id)))))
                True
          result `shouldBe` Right "second"
