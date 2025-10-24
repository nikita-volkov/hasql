module Sharing.ByUnit.Encoders.EnumSpec (spec) where

import Data.Text.Encoding (encodeUtf8)
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Errors qualified as Errors
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Test.Hspec
import Prelude

spec :: SpecWith (Text, Word16)
spec = do
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
              (Encoders.param (Encoders.nonNullable (Encoders.enum Nothing enumName id)))
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
              (Encoders.param (Encoders.nonNullable (Encoders.enum Nothing enumName id)))
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.enum Nothing enumName (Just . id)))))
              True
        result `shouldBe` Right "happy"

  describe "Enums in composites" do
    it "encodes enums nested in named composites" \config -> do
      enumName <- Scripts.generateSymname
      compositeName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create enum type
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["create type ", enumName, " as enum ('red', 'green', 'blue')"]))
              mempty
              Decoders.noResult
              True
          -- Create composite type with enum
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["create type ", compositeName, " as (id int8, color ", enumName, ")"]))
              mempty
              Decoders.noResult
              True
          -- Test encoding
          Session.statement (42 :: Int64, "green")
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select ($1 :: ", compositeName, ") = (42, 'green') :: ", compositeName]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.composite
                          Nothing
                          compositeName
                          ( divide
                              (\(a, b) -> (a, b))
                              (Encoders.field (Encoders.nonNullable Encoders.int8))
                              (Encoders.field (Encoders.nonNullable (Encoders.enum Nothing enumName id)))
                          )
                      )
                  )
              )
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
              True
        result `shouldBe` Right True

    it "roundtrips enums nested in named composites" \config -> do
      enumName <- Scripts.generateSymname
      compositeName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create enum type
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["create type ", enumName, " as enum ('red', 'green', 'blue')"]))
              mempty
              Decoders.noResult
              True
          -- Create composite type with enum
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["create type ", compositeName, " as (id int8, color ", enumName, ")"]))
              mempty
              Decoders.noResult
              True
          -- Test roundtrip
          Session.statement (42 :: Int64, "blue")
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select $1 :: ", compositeName]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.composite
                          Nothing
                          compositeName
                          ( divide
                              (\(a, b) -> (a, b))
                              (Encoders.field (Encoders.nonNullable Encoders.int8))
                              (Encoders.field (Encoders.nonNullable (Encoders.enum Nothing enumName id)))
                          )
                      )
                  )
              )
              ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          ( Decoders.composite
                              Nothing
                              compositeName
                              ( (,)
                                  <$> Decoders.field (Decoders.nonNullable Decoders.int8)
                                  <*> Decoders.field (Decoders.nonNullable (Decoders.enum Nothing enumName (Just . id)))
                              )
                          )
                      )
                  )
              )
              True
        result `shouldBe` Right (42 :: Int64, "blue")

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
                              (Encoders.element (Encoders.nonNullable (Encoders.enum Nothing enumName id)))
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
                              (Encoders.element (Encoders.nonNullable (Encoders.enum Nothing enumName id)))
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
                                  (Decoders.element (Decoders.nonNullable (Decoders.enum Nothing enumName (Just . id))))
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
              (Encoders.param (Encoders.nonNullable (Encoders.enum Nothing enumName id)))
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.enum Nothing enumName (Just . id)))))
              True
        result `shouldBe` Right "second"

  it "handles enum encoding and decoding" \config -> do
    name <- Scripts.generateSymname
    Scripts.onPreparableConnection config \connection -> do
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
            (Encoders.param (Encoders.nonNullable (Encoders.enum Nothing name id)))
            (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.enum Nothing name (Just . id)))))
            True
      result `shouldBe` Right "ok"

  it "detects attempts to encode non-existent enum types" \config -> do
    Scripts.onPreparableConnection config \connection -> do
      result <- Connection.use connection do
        Session.statement "test_value"
          $ Statement.Statement
            "select $1"
            (Encoders.param (Encoders.nonNullable (Encoders.enum Nothing "this_enum_does_not_exist_in_db" id)))
            (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
            True

      -- The statement should fail when trying to use a non-existent type
      -- This test documents the current behavior
      case result of
        Left err -> do
          -- We expect some kind of error
          err `shouldSatisfy` \case
            Errors.StatementSessionError {} -> True
            _ -> False
        Right _ ->
          expectationFailure "Expected error when using non-existent enum type, but statement succeeded"

  describe "Namespaced" do
    it "detects attempts to use non-existent type in non-existent schema" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          Session.statement "test"
            $ Statement.Statement
              "select $1::nonexistent_schema.nonexistent_type"
              (Encoders.param (Encoders.nonNullable (Encoders.enum (Just "nonexistent_schema") "nonexistent_type" id)))
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
              True

        -- The statement should fail when trying to use a non-existent schema.type
        case result of
          Left err -> do
            err `shouldSatisfy` \case
              Errors.StatementSessionError {} -> True
              _ -> False
          Right _ ->
            expectationFailure "Expected error when using non-existent schema.type, but statement succeeded"

    it "detects attempts to use non-existent type in existing schema" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          Session.statement "test"
            $ Statement.Statement
              "select $1::public.this_type_does_not_exist"
              (Encoders.param (Encoders.nonNullable (Encoders.enum (Just "public") "this_type_does_not_exist" id)))
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
              True

        -- The statement should fail when trying to use a non-existent type in existing schema
        case result of
          Left err -> do
            err `shouldSatisfy` \case
              Errors.StatementSessionError {} -> True
              _ -> False
          Right _ ->
            expectationFailure "Expected error when using non-existent type in existing schema, but statement succeeded"

  it "detects attempts to encode arrays of non-existent enum types" \config -> do
    Scripts.onPreparableConnection config \connection -> do
      result <- Connection.use connection do
        Session.statement ["val1", "val2"]
          $ Statement.Statement
            "select $1::nonexistent_array_enum[]"
            ( Encoders.param
                ( Encoders.nonNullable
                    ( Encoders.array
                        ( Encoders.dimension
                            foldl'
                            (Encoders.element (Encoders.nonNullable (Encoders.enum Nothing "nonexistent_array_enum" id)))
                        )
                    )
                )
            )
            (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
            True

      -- The statement should fail when trying to use a non-existent type
      case result of
        Left err -> do
          err `shouldSatisfy` \case
            Errors.StatementSessionError {} -> True
            _ -> False
        Right _ ->
          expectationFailure "Expected error when using array of non-existent enum type, but statement succeeded"
