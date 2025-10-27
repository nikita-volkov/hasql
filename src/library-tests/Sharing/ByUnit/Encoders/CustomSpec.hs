module Sharing.ByUnit.Encoders.CustomSpec (spec) where

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
  describe "Basic custom encoders" do
    it "encodes a custom type with runtime OID lookup" \config -> do
      enumName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create enum type
          Session.statement ()
            $ Statement.preparable
              (encodeUtf8 (mconcat ["create type ", enumName, " as enum ('alpha', 'beta', 'gamma')"]))
              mempty
              Decoders.noResult
          -- Test custom encoder with runtime OID lookup
          Session.statement "beta"
            $ Statement.preparable
              (encodeUtf8 (mconcat ["select ($1 :: ", enumName, ") = 'beta' :: ", enumName]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.custom
                          Nothing
                          enumName
                          Nothing
                          []
                          (\_ val -> encodeUtf8 val)
                          TextBuilder.text
                      )
                  )
              )
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        result `shouldBe` Right True

    it "encodes a custom type with static OIDs" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Test custom encoder with static OIDs for text (type OID 25, array OID 1009)
          Session.statement "hello"
            $ Statement.preparable
              "select $1::text = 'hello'::text"
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.custom
                          Nothing
                          "text"
                          (Just (25, 1009))
                          []
                          (\_ val -> encodeUtf8 val)
                          TextBuilder.text
                      )
                  )
              )
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        result `shouldBe` Right True

    it "encodes with dependent type OID requests" \config -> do
      enumName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create enum type
          Session.statement ()
            $ Statement.preparable
              (encodeUtf8 (mconcat ["create type ", enumName, " as enum ('small', 'large')"]))
              mempty
              Decoders.noResult
          -- Test custom encoder that requests OID of the enum type itself
          Session.statement "large"
            $ Statement.preparable
              (encodeUtf8 (mconcat ["select ($1 :: ", enumName, ") = 'large' :: ", enumName]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.custom
                          Nothing
                          enumName
                          Nothing
                          [(Nothing, enumName)]
                          ( \lookupOid val -> do
                              let (enumOidScalar, _) = lookupOid (Nothing, enumName)
                              -- Verify we got a valid OID (non-zero)
                              if enumOidScalar > 0
                                then encodeUtf8 val
                                else error "Failed to resolve enum OID"
                          )
                          TextBuilder.text
                      )
                  )
              )
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        result `shouldBe` Right True

  describe "Error handling" do
    it "detects missing types in custom encoders" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          Session.statement "test_value"
            $ Statement.preparable
              "select $1"
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.custom
                          Nothing
                          "nonexistent_custom_type"
                          Nothing
                          []
                          (\_ val -> encodeUtf8 val)
                          TextBuilder.text
                      )
                  )
              )
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))

        case result of
          Left (Errors.MissingTypesSessionError missingTypes) ->
            missingTypes `shouldBe` HashSet.fromList [(Nothing, "nonexistent_custom_type")]
          _ ->
            expectationFailure ("Unexpected result: " <> show result)

    it "detects missing dependent types in custom encoders" \config -> do
      customTypeName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create a custom type
          Session.statement ()
            $ Statement.preparable
              (encodeUtf8 (mconcat ["create type ", customTypeName, " as (id int4)"]))
              mempty
              Decoders.noResult
          -- Try to encode it but request a non-existent dependent type
          Session.statement (42 :: Int32)
            $ Statement.preparable
              (encodeUtf8 (mconcat ["select $1 :: ", customTypeName]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.custom
                          Nothing
                          customTypeName
                          Nothing
                          [(Nothing, "nonexistent_dependency")]
                          (\_ val -> encodeUtf8 (TextBuilder.toText (TextBuilder.decimal val)))
                          TextBuilder.decimal
                      )
                  )
              )
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))

        case result of
          Left (Errors.MissingTypesSessionError missingTypes) ->
            missingTypes `shouldBe` HashSet.fromList [(Nothing, "nonexistent_dependency")]
          _ ->
            expectationFailure ("Unexpected result: " <> show result)

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
          Session.statement "three"
            $ Statement.preparable
              (encodeUtf8 (mconcat ["select $1 :: ", enumName]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.custom
                          Nothing
                          enumName
                          Nothing
                          []
                          (\_ val -> encodeUtf8 val)
                          TextBuilder.text
                      )
                  )
              )
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
        result `shouldBe` Right "three"

    it "roundtrips multiple values" \config -> do
      enumName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create enum type
          Session.statement ()
            $ Statement.preparable
              (encodeUtf8 (mconcat ["create type ", enumName, " as enum ('first', 'second', 'third')"]))
              mempty
              Decoders.noResult
          -- Test roundtrip for multiple values
          r1 <-
            Session.statement "first"
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select $1 :: ", enumName]))
                ( Encoders.param
                    ( Encoders.nonNullable
                        ( Encoders.custom
                            Nothing
                            enumName
                            Nothing
                            []
                            (\_ val -> encodeUtf8 val)
                            TextBuilder.text
                        )
                    )
                )
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
          r2 <-
            Session.statement "third"
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select $1 :: ", enumName]))
                ( Encoders.param
                    ( Encoders.nonNullable
                        ( Encoders.custom
                            Nothing
                            enumName
                            Nothing
                            []
                            (\_ val -> encodeUtf8 val)
                            TextBuilder.text
                        )
                    )
                )
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
          return (r1, r2)
        result `shouldBe` Right ("first", "third")

  describe "Schema-qualified types" do
    it "encodes custom types from specific schemas" \config -> do
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
              (encodeUtf8 (mconcat ["create type ", schemaName, ".", typeName, " as enum ('x', 'y', 'z')"]))
              mempty
              Decoders.noResult
          -- Test custom encoder with schema qualification
          Session.statement "z"
            $ Statement.preparable
              (encodeUtf8 (mconcat ["select ($1 :: ", schemaName, ".", typeName, ") = 'z' :: ", schemaName, ".", typeName]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.custom
                          (Just schemaName)
                          typeName
                          Nothing
                          []
                          (\_ val -> encodeUtf8 val)
                          TextBuilder.text
                      )
                  )
              )
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        result `shouldBe` Right True

    it "detects missing types in non-existent schemas" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          Session.statement "test"
            $ Statement.preparable
              "select $1"
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.custom
                          (Just "nonexistent_schema")
                          "nonexistent_type"
                          Nothing
                          []
                          (\_ val -> encodeUtf8 val)
                          TextBuilder.text
                      )
                  )
              )
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))

        case result of
          Left (Errors.MissingTypesSessionError missingTypes) ->
            missingTypes `shouldBe` HashSet.fromList [(Just "nonexistent_schema", "nonexistent_type")]
          _ ->
            expectationFailure ("Unexpected result: " <> show result)
