module Sharing.ByUnit.Encoders.NamedCompositeSpec (spec) where

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
  describe "Named Composite Encoders" do
    describe "Simple composites" do
      it "encodes a simple named composite and compares with static value" \config -> do
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
            -- Test encoding by comparing with static value
            Session.statement (42 :: Int64, True)
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select ($1 :: ", typeName, ") = (42, true) :: ", typeName]))
                ( Encoders.param
                    ( Encoders.nonNullable
                        ( Encoders.namedComposite
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
                True
          result `shouldBe` Right True

      it "encodes and roundtrips a simple named composite" \config -> do
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
            -- Test roundtrip
            Session.statement (42 :: Int64, True)
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select $1 :: ", typeName]))
                ( Encoders.param
                    ( Encoders.nonNullable
                        ( Encoders.namedComposite
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
                            ( Decoders.namedComposite
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

    describe "Nested composites" do
      it "encodes nested named composites" \config -> do
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
            -- Test nested encoding
            Session.statement ((42 :: Int64, True), "hello")
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select ($1 :: ", outerType, ") = ((42, true), 'hello') :: ", outerType]))
                ( Encoders.param
                    ( Encoders.nonNullable
                        ( Encoders.namedComposite
                            Nothing
                            outerType
                            ( divide
                                (\(inner, z) -> (inner, z))
                                ( Encoders.field
                                    ( Encoders.nonNullable
                                        ( Encoders.namedComposite
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
                True
          result `shouldBe` Right True

      it "roundtrips nested named composites" \config -> do
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
            -- Test roundtrip
            Session.statement ((42 :: Int64, True), "hello")
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select $1 :: ", outerType]))
                ( Encoders.param
                    ( Encoders.nonNullable
                        ( Encoders.namedComposite
                            Nothing
                            outerType
                            ( divide
                                (\(inner, z) -> (inner, z))
                                ( Encoders.field
                                    ( Encoders.nonNullable
                                        ( Encoders.namedComposite
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
                            ( Decoders.namedComposite
                                Nothing
                                outerType
                                ( (,)
                                    <$> Decoders.field
                                      ( Decoders.nonNullable
                                          ( Decoders.namedComposite
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
          result `shouldBe` Right ((42 :: Int64, True), "hello")

    describe "Arrays of composites" do
      it "encodes arrays of named composites" \config -> do
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
            -- Test array encoding
            Session.statement [(1 :: Int64, True), (2, False), (3, True)]
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select $1 = array[(1, true), (2, false), (3, true)] :: ", typeName, "[]"]))
                ( Encoders.param
                    ( Encoders.nonNullable
                        ( Encoders.array
                            ( Encoders.dimension
                                foldl'
                                ( Encoders.element
                                    ( Encoders.nonNullable
                                        ( Encoders.namedComposite
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
                True
          result `shouldBe` Right True

      it "roundtrips arrays of named composites" \config -> do
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
            -- Test roundtrip
            Session.statement [(1 :: Int64, True), (2, False), (3, True)]
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select $1 :: ", typeName, "[]"]))
                ( Encoders.param
                    ( Encoders.nonNullable
                        ( Encoders.array
                            ( Encoders.dimension
                                foldl'
                                ( Encoders.element
                                    ( Encoders.nonNullable
                                        ( Encoders.namedComposite
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
                                            ( Decoders.namedComposite
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
