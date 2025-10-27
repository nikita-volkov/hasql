module Sharing.ByUnit.Decoders.CustomSpec (spec) where

import Data.ByteString qualified as ByteString
import Data.HashSet qualified as HashSet
import Data.Text.Encoding (encodeUtf8)
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Errors qualified as Errors
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Test.Hspec
import TextBuilder qualified
import Prelude

spec :: SpecWith (Text, Word16)
spec = do
  describe "Basic custom decoders" do
    it "decodes a custom type with runtime OID lookup" \config -> do
      enumName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create enum type
          Session.statement ()
            $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", enumName, " as enum ('alpha', 'beta', 'gamma')"]))
                mempty
                Decoders.noResult
          -- Test custom decoder with runtime OID lookup
          Session.statement ()
            $ Statement.preparable
                (encodeUtf8 (mconcat ["select 'beta' :: ", enumName]))
                mempty
                ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          ( Decoders.custom
                              Nothing
                              enumName
                              Nothing
                              []
                              (\_ bytes -> Right (ByteString.length bytes, bytes))
                          )
                      )
                  )
              )
        -- Should successfully decode with length and bytes
        case result of
          Right (len, bytes) -> do
            len `shouldBe` 4
            bytes `shouldBe` "beta"
          Left err ->
            expectationFailure ("Unexpected error: " <> show err)

    it "decodes a custom type with static OIDs" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Test custom decoder with static OIDs for int4 (type OID 23, array OID 1007)
          Session.statement ()
            $ Statement.preparable
                "select 42::int4"
                mempty
                ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          ( Decoders.custom
                              Nothing
                              "int4"
                              (Just (23, 1007))
                              []
                              (\_ bytes -> Right (ByteString.length bytes))
                          )
                      )
                  )
              )
        -- int4 is encoded in 4 bytes
        result `shouldBe` Right 4

    it "decodes with dependent type OID requests" \config -> do
      enumName <- Scripts.generateSymname
      compositeName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create enum type
          Session.statement ()
            $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", enumName, " as enum ('small', 'large')"]))
                mempty
                Decoders.noResult
          -- Create composite type with the enum
          Session.statement ()
            $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", compositeName, " as (size ", enumName, ", count int4)"]))
                mempty
                Decoders.noResult
          -- Test custom decoder requesting OIDs of dependent types
          Session.statement ()
            $ Statement.preparable
                (encodeUtf8 (mconcat ["select ('large', 5) :: ", compositeName]))
                mempty
                ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          ( Decoders.custom
                              Nothing
                              compositeName
                              Nothing
                              [(Nothing, enumName), (Nothing, "int4")]
                              ( \lookupOid bytes -> do
                                  let (enumOidScalar, _enumOidArray) = lookupOid (Nothing, enumName)
                                      (int4OidScalar, _int4OidArray) = lookupOid (Nothing, "int4")
                                  -- Verify we got valid OIDs
                                  if enumOidScalar > 0 && int4OidScalar > 0
                                    then Right (enumOidScalar, int4OidScalar, ByteString.length bytes)
                                    else Left "Failed to resolve OIDs"
                              )
                          )
                      )
                  )
              )
        -- Should successfully get OIDs and byte length
        case result of
          Right (enumOid, int4Oid, len) -> do
            enumOid `shouldSatisfy` (> 0)
            int4Oid `shouldBe` 23
            len `shouldSatisfy` (> 0)
          Left err ->
            expectationFailure ("Unexpected error: " <> show err)

  describe "Error handling" do
    it "detects missing types in custom decoders" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          Session.statement ()
            $ Statement.preparable
                "select 'test'::text"
                mempty
                ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          ( Decoders.custom
                              Nothing
                              "nonexistent_custom_type"
                              Nothing
                              []
                              (\_ bytes -> Right bytes)
                          )
                      )
                  )
              )

        case result of
          Left (Errors.MissingTypesSessionError missingTypes) ->
            missingTypes `shouldBe` HashSet.fromList [(Nothing, "nonexistent_custom_type")]
          _ ->
            expectationFailure ("Unexpected result: " <> show result)

    it "detects missing dependent types in custom decoders" \config -> do
      customTypeName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create a custom type
          Session.statement ()
            $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", customTypeName, " as (id int4)"]))
                mempty
                Decoders.noResult
          -- Try to decode it but request a non-existent dependent type
          Session.statement ()
            $ Statement.preparable
                (encodeUtf8 (mconcat ["select (42) :: ", customTypeName]))
                mempty
                ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          ( Decoders.custom
                              Nothing
                              customTypeName
                              Nothing
                              [(Nothing, "nonexistent_dependency")]
                              (\_ bytes -> Right bytes)
                          )
                      )
                  )
              )

        case result of
          Left (Errors.MissingTypesSessionError missingTypes) ->
            missingTypes `shouldBe` HashSet.fromList [(Nothing, "nonexistent_dependency")]
          _ ->
            expectationFailure ("Unexpected result: " <> show result)

    it "handles decoding errors in custom decoders" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          Session.statement ()
            $ Statement.preparable
                "select 42::int4"
                mempty
                ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          ( Decoders.custom
                              Nothing
                              "int4"
                              (Just (23, 1007))
                              []
                              (\_ _ -> Left "Custom decoding error")
                          )
                      )
                  )
              )

        case result of
          Left (Errors.StatementSessionError {}) -> pure ()
          _ ->
            expectationFailure "Expected statement error"

  describe "Roundtrip tests" do
    it "roundtrips custom encoded and decoded values" \config -> do
      enumName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create enum type
          Session.statement ()
            $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", enumName, " as enum ('one', 'two', 'three')"]))
                mempty
                Decoders.noResult
          -- Test roundtrip using custom encoder and decoder
          Session.statement "two"
            $ Statement.preparable
                (encodeUtf8 (mconcat ["select $1 :: ", enumName]))
                (Encoders.param (Encoders.nonNullable (Encoders.custom Nothing enumName Nothing [] (\_ val -> encodeUtf8 val) TextBuilder.text)))
                ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          ( Decoders.custom
                              Nothing
                              enumName
                              Nothing
                              []
                              (\_ bytes -> Right bytes)
                          )
                      )
                  )
              )
        result `shouldBe` Right "two"

  describe "Schema-qualified types" do
    it "decodes custom types from specific schemas" \config -> do
      schemaName <- Scripts.generateSymname
      typeName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create schema
          Session.statement ()
            $ Statement.preparable
                (encodeUtf8 (mconcat ["create schema ", schemaName]))
                mempty
                Decoders.noResult
          -- Create enum type in that schema
          Session.statement ()
            $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", schemaName, ".", typeName, " as enum ('x', 'y')"]))
                mempty
                Decoders.noResult
          -- Test custom decoder with schema qualification
          Session.statement ()
            $ Statement.preparable
                (encodeUtf8 (mconcat ["select 'y' :: ", schemaName, ".", typeName]))
                mempty
                ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          ( Decoders.custom
                              (Just schemaName)
                              typeName
                              Nothing
                              []
                              (\_ bytes -> Right bytes)
                          )
                      )
                  )
              )
        result `shouldBe` Right "y"
