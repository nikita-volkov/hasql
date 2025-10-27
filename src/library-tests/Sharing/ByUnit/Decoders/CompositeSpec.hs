module Sharing.ByUnit.Decoders.CompositeSpec (spec) where

import Data.HashSet qualified as HashSet
import Data.Text.Encoding (encodeUtf8)
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Errors qualified as Errors
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Test.Hspec
import Prelude

spec :: SpecWith (Text, Word16)
spec = do
  describe "Named Composite Decoders" do
    describe "Simple composites" do
      it "decodes a simple named composite from static SQL" \config -> do
        typeName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create composite type
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", typeName, " as (x int8, y bool)"]))
                mempty
                Decoders.noResult
            -- Test decoding from static value
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select (42, true) :: ", typeName]))
                mempty
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

      it "decodes a simple named composite with different values" \config -> do
        typeName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create composite type
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", typeName, " as (a text, b int4)"]))
                mempty
                Decoders.noResult
            -- Test decoding
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select ('hello', 123) :: ", typeName]))
                mempty
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            ( Decoders.composite
                                Nothing
                                typeName
                                ( (,)
                                    <$> Decoders.field (Decoders.nonNullable Decoders.text)
                                    <*> Decoders.field (Decoders.nonNullable Decoders.int4)
                                )
                            )
                        )
                    )
                )
          result `shouldBe` Right ("hello", 123 :: Int32)

    describe "Nested composites" do
      it "decodes nested named composites from static SQL" \config -> do
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
            -- Test nested decoding
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select ((42, true), 'world') :: ", outerType]))
                mempty
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
          result `shouldBe` Right ((42 :: Int64, True), "world")

      it "decodes deeply nested named composites" \config -> do
        type1 <- Scripts.generateSymname
        type2 <- Scripts.generateSymname
        type3 <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create level 1 composite
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", type1, " as (val int8)"]))
                mempty
                Decoders.noResult
            -- Create level 2 composite
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", type2, " as (\"inner\" ", type1, ", flag bool)"]))
                mempty
                Decoders.noResult
            -- Create level 3 composite
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", type3, " as (\"nested\" ", type2, ", name text)"]))
                mempty
                Decoders.noResult
            -- Test deeply nested decoding
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select row (row (row (99), true), 'deep') :: ", type3]))
                mempty
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            ( Decoders.composite
                                Nothing
                                type3
                                ( (,)
                                    <$> Decoders.field
                                      ( Decoders.nonNullable
                                          ( Decoders.composite
                                              Nothing
                                              type2
                                              ( (,)
                                                  <$> Decoders.field
                                                    ( Decoders.nonNullable
                                                        ( Decoders.composite
                                                            Nothing
                                                            type1
                                                            (Decoders.field (Decoders.nonNullable Decoders.int8))
                                                        )
                                                    )
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
          result `shouldBe` Right ((99 :: Int64, True), "deep")

    describe "Arrays of composites" do
      it "decodes arrays of primitives" \config -> do
        typeName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", typeName, " as (x int4[])"]))
                mempty
                Decoders.noResult
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select row(array[1,2,3])", " :: ", typeName]))
                mempty
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            ( Decoders.composite
                                Nothing
                                typeName
                                ( Decoders.field
                                    ( Decoders.nonNullable
                                        ( Decoders.array
                                            ( Decoders.dimension
                                                replicateM
                                                ( Decoders.element
                                                    (Decoders.nonNullable Decoders.int4)
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
          result `shouldBe` Right [1, 2, 3]

      it "decodes arrays of named composites from static SQL" \config -> do
        typeName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create composite type
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", typeName, " as (x int8, y bool)"]))
                mempty
                Decoders.noResult
            -- Test array decoding
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select array[(1, true), (2, false), (3, true)] :: ", typeName, "[]"]))
                mempty
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

      it "decodes 2D arrays of named composites" \config -> do
        typeName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create composite type
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", typeName, " as (val int4)"]))
                mempty
                Decoders.noResult
            -- Test 2D array decoding
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select array[array[row (1), row (2)], array[row (3), row (4)]] :: ", typeName, "[][]"]))
                mempty
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            ( Decoders.array
                                ( Decoders.dimension
                                    replicateM
                                    ( Decoders.dimension
                                        replicateM
                                        ( Decoders.element
                                            ( Decoders.nonNullable
                                                ( Decoders.composite
                                                    Nothing
                                                    typeName
                                                    (Decoders.field (Decoders.nonNullable Decoders.int4))
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
          result `shouldBe` Right [[1 :: Int32, 2], [3, 4]]

    describe "Composites with array fields" do
      it "decodes a composite with an enum array field" \config -> do
        enumType <- Scripts.generateSymname
        compositeType <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create enum type
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", enumType, " as enum ('red', 'green', 'blue')"]))
                mempty
                Decoders.noResult
            -- Create composite type with enum array field
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", compositeType, " as (id int8, colors ", enumType, "[])"]))
                mempty
                Decoders.noResult
            -- Test decoding composite with enum array field
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select (42, array['red', 'green', 'blue'] :: ", enumType, "[]) :: ", compositeType]))
                mempty
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            ( Decoders.composite
                                Nothing
                                compositeType
                                ( (,)
                                    <$> Decoders.field (Decoders.nonNullable Decoders.int8)
                                    <*> Decoders.field
                                      ( Decoders.nonNullable
                                          ( Decoders.array
                                              ( Decoders.dimension
                                                  replicateM
                                                  ( Decoders.element
                                                      ( Decoders.nonNullable
                                                          ( Decoders.enum
                                                              Nothing
                                                              enumType
                                                              Just
                                                          )
                                                      )
                                                  )
                                              )
                                          )
                                      )
                                )
                            )
                        )
                    )
                )
          result `shouldBe` Right (42 :: Int64, ["red", "green", "blue"])

      it "decodes a composite with multiple enum array fields" \config -> do
        enum1 <- Scripts.generateSymname
        enum2 <- Scripts.generateSymname
        compositeType <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create first enum type
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", enum1, " as enum ('small', 'medium', 'large')"]))
                mempty
                Decoders.noResult
            -- Create second enum type
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", enum2, " as enum ('low', 'high')"]))
                mempty
                Decoders.noResult
            -- Create composite type with multiple enum array fields
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", compositeType, " as (sizes ", enum1, "[], priorities ", enum2, "[])"]))
                mempty
                Decoders.noResult
            -- Test decoding composite with multiple enum array fields
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select (array['small', 'large'] :: ", enum1, "[], array['high', 'low'] :: ", enum2, "[]) :: ", compositeType]))
                mempty
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            ( Decoders.composite
                                Nothing
                                compositeType
                                ( (,)
                                    <$> Decoders.field
                                      ( Decoders.nonNullable
                                          ( Decoders.array
                                              ( Decoders.dimension
                                                  replicateM
                                                  ( Decoders.element
                                                      ( Decoders.nonNullable
                                                          ( Decoders.enum
                                                              Nothing
                                                              enum1
                                                              Just
                                                          )
                                                      )
                                                  )
                                              )
                                          )
                                      )
                                    <*> Decoders.field
                                      ( Decoders.nonNullable
                                          ( Decoders.array
                                              ( Decoders.dimension
                                                  replicateM
                                                  ( Decoders.element
                                                      ( Decoders.nonNullable
                                                          ( Decoders.enum
                                                              Nothing
                                                              enum2
                                                              Just
                                                          )
                                                      )
                                                  )
                                              )
                                          )
                                      )
                                )
                            )
                        )
                    )
                )
          result `shouldBe` Right (["small", "large"], ["high", "low"])

      it "decodes a composite with mixed scalar and enum array fields" \config -> do
        enumType <- Scripts.generateSymname
        compositeType <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create enum type
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", enumType, " as enum ('A', 'B', 'C')"]))
                mempty
                Decoders.noResult
            -- Create composite type with mixed fields
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", compositeType, " as (name text, age int4, grades ", enumType, "[])"]))
                mempty
                Decoders.noResult
            -- Test decoding composite with mixed fields
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select ('Alice', 25, array['A', 'B', 'A'] :: ", enumType, "[]) :: ", compositeType]))
                mempty
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            ( Decoders.composite
                                Nothing
                                compositeType
                                ( do
                                    name <- Decoders.field (Decoders.nonNullable Decoders.text)
                                    age <- Decoders.field (Decoders.nonNullable Decoders.int4)
                                    grades <-
                                      Decoders.field
                                        ( Decoders.nonNullable
                                            ( Decoders.array
                                                ( Decoders.dimension
                                                    replicateM
                                                    ( Decoders.element
                                                        ( Decoders.nonNullable
                                                            ( Decoders.enum
                                                                Nothing
                                                                enumType
                                                                ( \case
                                                                    "A" -> Just 'A'
                                                                    "B" -> Just 'B'
                                                                    "C" -> Just 'C'
                                                                    _ -> Nothing
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    pure (name, age, grades)
                                )
                            )
                        )
                    )
                )
          result `shouldBe` Right ("Alice", 25 :: Int32, ['A', 'B', 'A'])

      it "decodes nested composite with enum array field" \config -> do
        enumType <- Scripts.generateSymname
        innerType <- Scripts.generateSymname
        outerType <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create enum type
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", enumType, " as enum ('x', 'y', 'z')"]))
                mempty
                Decoders.noResult
            -- Create inner composite type with enum array field
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", innerType, " as (values ", enumType, "[])"]))
                mempty
                Decoders.noResult
            -- Create outer composite type containing the inner type
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", outerType, " as (id int4, data ", innerType, ")"]))
                mempty
                Decoders.noResult
            -- Test nested decoding
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select (100, row(array['x', 'y', 'z'] :: ", enumType, "[]) :: ", innerType, ") :: ", outerType]))
                mempty
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            ( Decoders.composite
                                Nothing
                                outerType
                                ( (,)
                                    <$> Decoders.field (Decoders.nonNullable Decoders.int4)
                                    <*> Decoders.field
                                      ( Decoders.nonNullable
                                          ( Decoders.composite
                                              Nothing
                                              innerType
                                              ( Decoders.field
                                                  ( Decoders.nonNullable
                                                      ( Decoders.array
                                                          ( Decoders.dimension
                                                              replicateM
                                                              ( Decoders.element
                                                                  ( Decoders.nonNullable
                                                                      ( Decoders.enum
                                                                          Nothing
                                                                          enumType
                                                                          ( \case
                                                                              "x" -> Just 'x'
                                                                              "y" -> Just 'y'
                                                                              "z" -> Just 'z'
                                                                              _ -> Nothing
                                                                          )
                                                                      )
                                                                  )
                                                              )
                                                          )
                                                      )
                                                  )
                                              )
                                          )
                                      )
                                )
                            )
                        )
                    )
                )
          result `shouldBe` Right (100 :: Int32, ['x', 'y', 'z'])

      it "decodes a composite with 2D enum array field" \config -> do
        enumType <- Scripts.generateSymname
        compositeType <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create enum type
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", enumType, " as enum ('0', '1')"]))
                mempty
                Decoders.noResult
            -- Create composite type with 2D enum array field
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", compositeType, " as (matrix ", enumType, "[][])"]))
                mempty
                Decoders.noResult
            -- Test decoding composite with 2D enum array field
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select row(array[array['0', '1'], array['1', '0']] :: ", enumType, "[][]) :: ", compositeType]))
                mempty
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            ( Decoders.composite
                                Nothing
                                compositeType
                                ( Decoders.field
                                    ( Decoders.nonNullable
                                        ( Decoders.array
                                            ( Decoders.dimension
                                                replicateM
                                                ( Decoders.dimension
                                                    replicateM
                                                    ( Decoders.element
                                                        ( Decoders.nonNullable
                                                            ( Decoders.enum
                                                                Nothing
                                                                enumType
                                                                ( \case
                                                                    "0" -> Just (0 :: Int)
                                                                    "1" -> Just 1
                                                                    _ -> Nothing
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
          result `shouldBe` Right [[0, 1], [1, 0]]

    describe "OID compatibility checking" do
      it "fails when decoder expects a composite but gets a different type" \config -> do
        typeName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create composite type
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", typeName, " as (x int8, y bool)"]))
                mempty
                Decoders.noResult
            -- Try to decode text as the composite type (should fail during deserialization)
            Session.statement ()
              $ Statement.preparable
                "select 'some text'::text"
                mempty
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
          -- Should fail with a cell error because text cannot be decoded as a composite
          case result of
            Left (Errors.StatementSessionError _ _ _ _ _ (Errors.UnexpectedColumnTypeStatementError 0 _ _)) ->
              pure ()
            Left err ->
              expectationFailure ("Unexpected type of error: " <> show err)
            Right _ ->
              expectationFailure "Expected an error but got success"

      it "fails when decoder expects one composite type but gets another" \config -> do
        type1 <- Scripts.generateSymname
        type2 <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create first composite type with two fields
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", type1, " as (x int8, y text)"]))
                mempty
                Decoders.noResult
            -- Create second composite type with different structure
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", type2, " as (a bool)"]))
                mempty
                Decoders.noResult
            -- Try to decode type2 value as type1 (should fail during deserialization)
            -- type2 has 1 field, type1 decoder expects 2 fields
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select row (true) :: ", type2]))
                mempty
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            ( Decoders.composite
                                Nothing
                                type1
                                ( (,)
                                    <$> Decoders.field (Decoders.nonNullable Decoders.int8)
                                    <*> Decoders.field (Decoders.nonNullable Decoders.text)
                                )
                            )
                        )
                    )
                )
          -- Should fail with a cell error because the field count doesn't match
          case result of
            Left (Errors.StatementSessionError _ _ _ _ _ (Errors.UnexpectedColumnTypeStatementError 0 _ _)) ->
              pure ()
            Left err ->
              expectationFailure ("Unexpected type of error: " <> show err)
            Right _ ->
              expectationFailure "Expected an error but got success"

      it "correctly validates matching composite type OIDs" \config -> do
        typeName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create composite type
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", typeName, " as (x int8, y bool)"]))
                mempty
                Decoders.noResult
            -- Decode with correct type - should succeed
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select row (42, true) :: ", typeName]))
                mempty
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

  it "detects attempts to decode non-existent composite types" \config -> do
    Scripts.onPreparableConnection config \connection -> do
      result <- Connection.use connection do
        Session.statement ()
          $ Statement.preparable
            "select row(42, text 'test')"
            mempty
            ( Decoders.singleRow
                ( Decoders.column
                    ( Decoders.nonNullable
                        ( Decoders.composite
                            Nothing
                            "nonexistent_composite_type"
                            ( (,)
                                <$> Decoders.field (Decoders.nonNullable Decoders.int4)
                                <*> Decoders.field (Decoders.nonNullable Decoders.text)
                            )
                        )
                    )
                )
            )

      case result of
        Left (Errors.MissingTypesSessionError missingTypes) ->
          missingTypes `shouldBe` HashSet.fromList [(Nothing, "nonexistent_composite_type")]
        _ ->
          expectationFailure ("Unexpected result: " <> show result)
