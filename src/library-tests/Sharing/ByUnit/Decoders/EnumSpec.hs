module Sharing.ByUnit.Decoders.EnumSpec (spec) where

import Data.Text.Encoding (encodeUtf8)
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Test.Hspec
import Prelude

spec :: SpecWith (Text, Word16)
spec = do
  describe "Named Enum Decoders" do
    describe "Simple enums" do
      it "decodes a simple named enum from static SQL" \config -> do
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
            -- Test decoding from static value
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select 'happy' :: ", enumName]))
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.enum Nothing enumName (Just . id)))))
                True
          result `shouldBe` Right "happy"

      it "decodes different enum values" \config -> do
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
            -- Test decoding multiple values
            r1 <-
              Session.statement ()
                $ Statement.Statement
                  (encodeUtf8 (mconcat ["select 'alpha' :: ", enumName]))
                  mempty
                  (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.enum Nothing enumName (Just . id)))))
                  True
            r2 <-
              Session.statement ()
                $ Statement.Statement
                  (encodeUtf8 (mconcat ["select 'gamma' :: ", enumName]))
                  mempty
                  (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.enum Nothing enumName (Just . id)))))
                  True
            return (r1, r2)
          result `shouldBe` Right ("alpha", "gamma")

    describe "Enums in composites" do
      it "decodes enums nested in named composites from static SQL" \config -> do
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
            -- Test decoding
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select (42, 'green') :: ", compositeName]))
                mempty
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
          result `shouldBe` Right (42 :: Int64, "green")

      it "decodes multiple levels of nesting with enums" \config -> do
        enumName <- Scripts.generateSymname
        innerType <- Scripts.generateSymname
        outerType <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create enum type
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", enumName, " as enum ('small', 'medium', 'large')"]))
                mempty
                Decoders.noResult
                True
            -- Create inner composite with enum
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", innerType, " as (size ", enumName, ", count int4)"]))
                mempty
                Decoders.noResult
                True
            -- Create outer composite
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", outerType, " as (\"inner\" ", innerType, ", name text)"]))
                mempty
                Decoders.noResult
                True
            -- Test decoding
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select (('large', 5), 'test') :: ", outerType]))
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
                                                  <$> Decoders.field (Decoders.nonNullable (Decoders.enum Nothing enumName (Just . id)))
                                                  <*> Decoders.field (Decoders.nonNullable Decoders.int4)
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
          result `shouldBe` Right (("large", 5 :: Int32), "test")

    describe "Arrays of enums" do
      it "decodes arrays of named enums from static SQL" \config -> do
        enumName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create enum type
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", enumName, " as enum ('first', 'second', 'third')"]))
                mempty
                Decoders.noResult
                True
            -- Test array decoding
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select array['first', 'third', 'second'] :: ", enumName, "[]"]))
                mempty
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
          result `shouldBe` Right ["first", "third", "second"]

      it "decodes 2D arrays of named enums" \config -> do
        enumName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create enum type
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", enumName, " as enum ('a', 'b', 'c')"]))
                mempty
                Decoders.noResult
                True
            -- Test 2D array decoding
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select array[array['a', 'b'], array['c', 'a']] :: ", enumName, "[][]"]))
                mempty
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            ( Decoders.array
                                ( Decoders.dimension
                                    replicateM
                                    ( Decoders.dimension
                                        replicateM
                                        (Decoders.element (Decoders.nonNullable (Decoders.enum Nothing enumName (Just . id))))
                                    )
                                )
                            )
                        )
                    )
                )
                True
          result `shouldBe` Right [["a", "b"], ["c", "a"]]

      it "decodes arrays of composites containing enums" \config -> do
        enumName <- Scripts.generateSymname
        compositeName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create enum type
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", enumName, " as enum ('low', 'high')"]))
                mempty
                Decoders.noResult
                True
            -- Create composite type with enum
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", compositeName, " as (priority ", enumName, ", id int4)"]))
                mempty
                Decoders.noResult
                True
            -- Test decoding array of composites with enums
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select array[('high', 1), ('low', 2)] :: ", compositeName, "[]"]))
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
                                                compositeName
                                                ( (,)
                                                    <$> Decoders.field (Decoders.nonNullable (Decoders.enum Nothing enumName (Just . id)))
                                                    <*> Decoders.field (Decoders.nonNullable Decoders.int4)
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
          result `shouldBe` Right [("high", 1 :: Int32), ("low", 2)]
