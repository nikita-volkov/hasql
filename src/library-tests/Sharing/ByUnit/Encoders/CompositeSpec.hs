module Sharing.ByUnit.Encoders.CompositeSpec (spec) where

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
import Prelude

spec :: SpecWith (Text, Word16)
spec = do
  describe "Simple composites" do
    it "encodes a simple named composite and compares with static value" \config -> do
      typeName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create composite type
          Session.statement ()
            $ Statement.preparable
              (encodeUtf8 (mconcat ["create type ", typeName, " as (x int8, y bool)"]))
              mempty
              Decoders.noResult
          -- Test encoding by comparing with static value
          Session.statement (42 :: Int64, True)
            $ Statement.preparable
              (encodeUtf8 (mconcat ["select ($1 :: ", typeName, ") = (42, true) :: ", typeName]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.composite
                          Nothing
                          typeName
                          ( divide
                              (\(a, b) -> (a, b))
                              (Encoders.field (Encoders.nonNullable Encoders.int8))
                              (Encoders.field (Encoders.nonNullable Encoders.bool))
                          )
                      )
                  )
              )
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        result `shouldBe` Right True

    it "encodes and roundtrips a simple named composite" \config -> do
      typeName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create composite type
          Session.statement ()
            $ Statement.preparable
              (encodeUtf8 (mconcat ["create type ", typeName, " as (x int8, y bool)"]))
              mempty
              Decoders.noResult
          -- Test roundtrip
          Session.statement (42 :: Int64, True)
            $ Statement.preparable
              (encodeUtf8 (mconcat ["select $1 :: ", typeName]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.composite
                          Nothing
                          typeName
                          ( divide
                              (\(a, b) -> (a, b))
                              (Encoders.field (Encoders.nonNullable Encoders.int8))
                              (Encoders.field (Encoders.nonNullable Encoders.bool))
                          )
                      )
                  )
              )
              ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          ( Decoders.composite
                              Nothing
                              typeName
                              ( (,)
                                  <$> Decoders.field (Decoders.nonNullable Decoders.int8)
                                  <*> Decoders.field (Decoders.nonNullable Decoders.bool)
                              )
                          )
                      )
                  )
              )
        result `shouldBe` Right (42 :: Int64, True)

  describe "Nested composites" do
    it "encodes nested named composites" \config -> do
      innerType <- Scripts.generateSymname
      outerType <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create inner composite type
          Session.statement ()
            $ Statement.preparable
              (encodeUtf8 (mconcat ["create type ", innerType, " as (x int8, y bool)"]))
              mempty
              Decoders.noResult
          -- Create outer composite type
          Session.statement ()
            $ Statement.preparable
              (encodeUtf8 (mconcat ["create type ", outerType, " as (\"inner\" ", innerType, ", z text)"]))
              mempty
              Decoders.noResult
          -- Test nested encoding
          Session.statement ((42 :: Int64, True), "hello")
            $ Statement.preparable
              (encodeUtf8 (mconcat ["select ($1 :: ", outerType, ") = ((42, true), 'hello') :: ", outerType]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.composite
                          Nothing
                          outerType
                          ( divide
                              (\(inner, z) -> (inner, z))
                              ( Encoders.field
                                  ( Encoders.nonNullable
                                      ( Encoders.composite
                                          Nothing
                                          innerType
                                          ( divide
                                              (\(a, b) -> (a, b))
                                              (Encoders.field (Encoders.nonNullable Encoders.int8))
                                              (Encoders.field (Encoders.nonNullable Encoders.bool))
                                          )
                                      )
                                  )
                              )
                              (Encoders.field (Encoders.nonNullable Encoders.text))
                          )
                      )
                  )
              )
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        result `shouldBe` Right True

    it "roundtrips nested named composites" \config -> do
      innerType <- Scripts.generateSymname
      outerType <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create inner composite type
          Session.statement ()
            $ Statement.preparable
              (encodeUtf8 (mconcat ["create type ", innerType, " as (x int8, y bool)"]))
              mempty
              Decoders.noResult
          -- Create outer composite type
          Session.statement ()
            $ Statement.preparable
              (encodeUtf8 (mconcat ["create type ", outerType, " as (\"inner\" ", innerType, ", z text)"]))
              mempty
              Decoders.noResult
          -- Test roundtrip
          Session.statement ((42 :: Int64, True), "hello")
            $ Statement.preparable
              (encodeUtf8 (mconcat ["select $1 :: ", outerType]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.composite
                          Nothing
                          outerType
                          ( divide
                              (\(inner, z) -> (inner, z))
                              ( Encoders.field
                                  ( Encoders.nonNullable
                                      ( Encoders.composite
                                          Nothing
                                          innerType
                                          ( divide
                                              (\(a, b) -> (a, b))
                                              (Encoders.field (Encoders.nonNullable Encoders.int8))
                                              (Encoders.field (Encoders.nonNullable Encoders.bool))
                                          )
                                      )
                                  )
                              )
                              (Encoders.field (Encoders.nonNullable Encoders.text))
                          )
                      )
                  )
              )
              ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          ( Decoders.composite
                              Nothing
                              outerType
                              ( (,)
                                  <$> Decoders.field
                                    ( Decoders.nonNullable
                                        ( Decoders.composite
                                            Nothing
                                            innerType
                                            ( (,)
                                                <$> Decoders.field (Decoders.nonNullable Decoders.int8)
                                                <*> Decoders.field (Decoders.nonNullable Decoders.bool)
                                            )
                                        )
                                    )
                                  <*> Decoders.field (Decoders.nonNullable Decoders.text)
                              )
                          )
                      )
                  )
              )
        result `shouldBe` Right ((42 :: Int64, True), "hello")

  describe "Arrays of composites" do
    it "encodes arrays of named composites" \config -> do
      typeName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create composite type
          Session.statement ()
            $ Statement.preparable
              (encodeUtf8 (mconcat ["create type ", typeName, " as (x int8, y bool)"]))
              mempty
              Decoders.noResult
          -- Test array encoding
          Session.statement [(1 :: Int64, True), (2, False), (3, True)]
            $ Statement.preparable
              (encodeUtf8 (mconcat ["select $1 = array[(1, true), (2, false), (3, true)] :: ", typeName, "[]"]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.array
                          ( Encoders.dimension
                              foldl'
                              ( Encoders.element
                                  ( Encoders.nonNullable
                                      ( Encoders.composite
                                          Nothing
                                          typeName
                                          ( divide
                                              (\(a, b) -> (a, b))
                                              (Encoders.field (Encoders.nonNullable Encoders.int8))
                                              (Encoders.field (Encoders.nonNullable Encoders.bool))
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
              )
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        result `shouldBe` Right True

    it "roundtrips arrays of named composites" \config -> do
      typeName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create composite type
          Session.statement ()
            $ Statement.preparable
              (encodeUtf8 (mconcat ["create type ", typeName, " as (x int8, y bool)"]))
              mempty
              Decoders.noResult
          -- Test roundtrip
          Session.statement [(1 :: Int64, True), (2, False), (3, True)]
            $ Statement.preparable
              (encodeUtf8 (mconcat ["select $1 :: ", typeName, "[]"]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.array
                          ( Encoders.dimension
                              foldl'
                              ( Encoders.element
                                  ( Encoders.nonNullable
                                      ( Encoders.composite
                                          Nothing
                                          typeName
                                          ( divide
                                              (\(a, b) -> (a, b))
                                              (Encoders.field (Encoders.nonNullable Encoders.int8))
                                              (Encoders.field (Encoders.nonNullable Encoders.bool))
                                          )
                                      )
                                  )
                              )
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
                                  ( Decoders.element
                                      ( Decoders.nonNullable
                                          ( Decoders.composite
                                              Nothing
                                              typeName
                                              ( (,)
                                                  <$> Decoders.field (Decoders.nonNullable Decoders.int8)
                                                  <*> Decoders.field (Decoders.nonNullable Decoders.bool)
                                              )
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
              )
        result `shouldBe` Right [(1 :: Int64, True), (2, False), (3, True)]

  describe "OID lookup verification" do
    it "requests OID for named composites (verified by successful execution)" \config -> do
      -- This test verifies that OID lookup happens by ensuring a named composite
      -- type works correctly - if OID lookup didn't happen, the statement would fail
      typeName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create composite type
          Session.statement ()
            $ Statement.preparable
              (encodeUtf8 (mconcat ["create type ", typeName, " as (value int8)"]))
              mempty
              Decoders.noResult
          -- Use named composite - this requires OID lookup to succeed
          Session.statement (100 :: Int64)
            $ Statement.preparable
              (encodeUtf8 (mconcat ["select $1 :: ", typeName]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.composite
                          Nothing
                          typeName
                          (Encoders.field (Encoders.nonNullable Encoders.int8))
                      )
                  )
              )
              ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          (Decoders.composite Nothing typeName (Decoders.field (Decoders.nonNullable Decoders.int8)))
                      )
                  )
              )
        result `shouldBe` Right (100 :: Int64)

    it "correctly tracks unknown types for nested composites with built-in field types" \config -> do
      -- This test reproduces the bug where unknownTypes were incorrectly tracked.
      -- The bug: when a field had a known elementOid (like int8), it was incorrectly
      -- added to unknownTypes. When elementOid was Nothing (custom types), it wasn't added.
      -- This caused nested composites with built-in types to fail OID lookup.
      --
      -- Specifically: When using a named composite as a field in another composite,
      -- the inner composite type needs OID lookup (it's custom), but its int8 field doesn't.
      -- The bug would cause int8 to be requested for OID lookup (wasteful but harmless)
      -- and fail to request OID lookup for the inner composite type (causing failure).
      innerType <- Scripts.generateSymname
      outerType <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create inner composite with a built-in type field
          Session.statement ()
            $ Statement.preparable
              (encodeUtf8 (mconcat ["create type ", innerType, " as (value int8)"]))
              mempty
              Decoders.noResult
          -- Create outer composite containing the inner composite
          Session.statement ()
            $ Statement.preparable
              (encodeUtf8 (mconcat ["create type ", outerType, " as (\"inner\" ", innerType, ")"]))
              mempty
              Decoders.noResult
          -- With the bug: innerType wouldn't be in the OID cache because
          -- field (with Nothing elementOid) didn't add it to unknownTypes.
          -- Instead, int8 (with Just elementOid) was being added (incorrectly).
          -- This would cause the encoder to use OID 0 for innerType, causing an error.
          Session.statement (42 :: Int64)
            $ Statement.preparable
              (encodeUtf8 (mconcat ["select ($1 :: ", outerType, ").inner.value"]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.composite
                          Nothing
                          outerType
                          ( Encoders.field
                              ( Encoders.nonNullable
                                  ( Encoders.composite
                                      Nothing
                                      innerType
                                      (Encoders.field (Encoders.nonNullable Encoders.int8))
                                  )
                              )
                          )
                      )
                  )
              )
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
        result `shouldBe` Right (42 :: Int64)

  describe "OID compatibility checking" do
    it "validates that encoder uses correct composite type OID" \config -> do
      -- This test ensures that when encoding a composite type, the correct OID is used.
      -- If the OID lookup fails or returns wrong OID, the statement should fail.
      typeName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create composite type
          Session.statement ()
            $ Statement.preparable
              (encodeUtf8 (mconcat ["create type ", typeName, " as (x int8, y bool)"]))
              mempty
              Decoders.noResult
          -- Encode and verify - the DB will validate the OID is correct
          Session.statement (42 :: Int64, True)
            $ Statement.preparable
              (encodeUtf8 (mconcat ["select ($1 :: ", typeName, ") = row (42, true) :: ", typeName]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.composite
                          Nothing
                          typeName
                          ( divide
                              (\(a, b) -> (a, b))
                              (Encoders.field (Encoders.nonNullable Encoders.int8))
                              (Encoders.field (Encoders.nonNullable Encoders.bool))
                          )
                      )
                  )
              )
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        result `shouldBe` Right True

    it "validates OID lookup for nested composite types during encoding" \config -> do
      -- This test ensures OID lookup works correctly for nested composites
      innerType <- Scripts.generateSymname
      outerType <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create inner composite type
          Session.statement ()
            $ Statement.preparable
              (encodeUtf8 (mconcat ["create type ", innerType, " as (value int8)"]))
              mempty
              Decoders.noResult
          -- Create outer composite type
          Session.statement ()
            $ Statement.preparable
              (encodeUtf8 (mconcat ["create type ", outerType, " as (\"nested\" ", innerType, ", flag bool)"]))
              mempty
              Decoders.noResult
          -- Encode nested composite - both type OIDs must be looked up correctly
          Session.statement (99 :: Int64, True)
            $ Statement.preparable
              (encodeUtf8 (mconcat ["select ($1 :: ", outerType, ") = row (row (99), true) :: ", outerType]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.composite
                          Nothing
                          outerType
                          ( divide
                              (\(val, flag) -> (val, flag))
                              ( Encoders.field
                                  ( Encoders.nonNullable
                                      ( Encoders.composite
                                          Nothing
                                          innerType
                                          (Encoders.field (Encoders.nonNullable Encoders.int8))
                                      )
                                  )
                              )
                              (Encoders.field (Encoders.nonNullable Encoders.bool))
                          )
                      )
                  )
              )
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        result `shouldBe` Right True

  it "detects attempts to encode non-existent composite types" \config -> do
    Scripts.onPreparableConnection config \connection -> do
      result <- Connection.use connection do
        Session.statement (42 :: Int64, "test")
          $ Statement.preparable
            "select $1::nonexistent_composite_type"
            ( Encoders.param
                ( Encoders.nonNullable
                    ( Encoders.composite
                        Nothing
                        "nonexistent_composite_type"
                        ( divide
                            (\(a, b) -> (a, b))
                            (Encoders.field (Encoders.nonNullable Encoders.int8))
                            (Encoders.field (Encoders.nonNullable Encoders.text))
                        )
                    )
                )
            )
            (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))

      case result of
        Left (Errors.MissingTypesSessionError missingTypes) ->
          missingTypes `shouldBe` HashSet.fromList [(Nothing, "nonexistent_composite_type")]
        _ ->
          expectationFailure ("Unexpected result: " <> show result)
