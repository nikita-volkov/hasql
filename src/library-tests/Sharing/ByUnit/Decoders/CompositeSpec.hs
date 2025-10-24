module Sharing.ByUnit.Decoders.CompositeSpec (spec) where

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
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", typeName, " as (x int8, y bool)"]))
                mempty
                Decoders.noResult
                True
            -- Test decoding from static value
            Session.statement ()
              $ Statement.Statement
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
                True
          result `shouldBe` Right (42 :: Int64, True)

      it "decodes a simple named composite with different values" \config -> do
        typeName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create composite type
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", typeName, " as (a text, b int4)"]))
                mempty
                Decoders.noResult
                True
            -- Test decoding
            Session.statement ()
              $ Statement.Statement
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
                True
          result `shouldBe` Right ("hello", 123 :: Int32)

    describe "Nested composites" do
      it "decodes nested named composites from static SQL" \config -> do
        innerType <- Scripts.generateSymname
        outerType <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create inner composite type
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", innerType, " as (x int8, y bool)"]))
                mempty
                Decoders.noResult
                True
            -- Create outer composite type
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", outerType, " as (\"inner\" ", innerType, ", z text)"]))
                mempty
                Decoders.noResult
                True
            -- Test nested decoding
            Session.statement ()
              $ Statement.Statement
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
                True
          result `shouldBe` Right ((42 :: Int64, True), "world")

      it "decodes deeply nested named composites" \config -> do
        type1 <- Scripts.generateSymname
        type2 <- Scripts.generateSymname
        type3 <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create level 1 composite
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", type1, " as (val int8)"]))
                mempty
                Decoders.noResult
                True
            -- Create level 2 composite
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", type2, " as (\"inner\" ", type1, ", flag bool)"]))
                mempty
                Decoders.noResult
                True
            -- Create level 3 composite
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", type3, " as (\"nested\" ", type2, ", name text)"]))
                mempty
                Decoders.noResult
                True
            -- Test deeply nested decoding
            Session.statement ()
              $ Statement.Statement
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
                True
          result `shouldBe` Right ((99 :: Int64, True), "deep")

    describe "Arrays of composites" do
      it "decodes arrays of named composites from static SQL" \config -> do
        typeName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create composite type
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", typeName, " as (x int8, y bool)"]))
                mempty
                Decoders.noResult
                True
            -- Test array decoding
            Session.statement ()
              $ Statement.Statement
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
                True
          result `shouldBe` Right [(1 :: Int64, True), (2, False), (3, True)]

      it "decodes 2D arrays of named composites" \config -> do
        typeName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create composite type
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", typeName, " as (val int4)"]))
                mempty
                Decoders.noResult
                True
            -- Test 2D array decoding
            Session.statement ()
              $ Statement.Statement
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
                True
          result `shouldBe` Right [[1 :: Int32, 2], [3, 4]]

  it "detects attempts to decode non-existent composite types" \config -> do
    Scripts.onPreparableConnection config \connection -> do
      result <- Connection.use connection do
        Session.statement ()
          $ Statement.Statement
            "select row(42, 'test')"
            mempty
            ( Decoders.singleRow
                ( Decoders.column
                    ( Decoders.nonNullable
                        ( Decoders.composite
                            Nothing
                            "nonexistent_composite_type"
                            ( (,)
                                <$> Decoders.field (Decoders.nonNullable Decoders.int8)
                                <*> Decoders.field (Decoders.nonNullable Decoders.text)
                            )
                        )
                    )
                )
            )
            True

      -- The statement should fail when trying to decode with a non-existent type
      case result of
        Left err -> do
          err `shouldSatisfy` \case
            Errors.StatementSessionError {} -> True
            _ -> False
        Right _ ->
          expectationFailure "Expected error when decoding with non-existent composite type, but statement succeeded"
