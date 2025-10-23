module Sharing.ByUnit.Encoders.UnnamedEnumSpec (spec) where

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
  describe "Unnamed Enum Encoders" do
    describe "Simple enums" do
      it "encodes a simple unnamed enum with explicit type cast" \config -> do
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
            -- Test encoding with explicit cast in SQL
            Session.statement "ok"
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select ($1 :: ", enumName, ") = 'ok' :: ", enumName]))
                (Encoders.param (Encoders.nonNullable (Encoders.unnamedEnum id)))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
                True
          result `shouldBe` Right True

      it "roundtrips a simple unnamed enum" \config -> do
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
            -- Test roundtrip with explicit cast
            Session.statement "gamma"
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select $1 :: ", enumName]))
                (Encoders.param (Encoders.nonNullable (Encoders.unnamedEnum id)))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.namedEnum Nothing enumName (Just . id)))))
                True
          result `shouldBe` Right "gamma"

    describe "Enums in composites" do
      it "encodes unnamed enums nested in composites" \config -> do
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
            -- Test encoding with explicit cast
            Session.statement (42 :: Int64, "green")
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select ($1 :: ", compositeName, ") = (42, 'green') :: ", compositeName]))
                ( Encoders.param
                    ( Encoders.nonNullable
                        ( Encoders.namedComposite
                            Nothing
                            compositeName
                            ( divide
                                (\(a, b) -> (a, b))
                                (Encoders.field (Encoders.nonNullable Encoders.int8))
                                (Encoders.field (Encoders.nonNullable (Encoders.unnamedEnum id)))
                            )
                        )
                    )
                )
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
                True
          result `shouldBe` Right True

      it "roundtrips unnamed enums nested in composites" \config -> do
        enumName <- Scripts.generateSymname
        compositeName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create enum type
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", enumName, " as enum ('small', 'medium', 'large')"]))
                mempty
                Decoders.noResult
                True
            -- Create composite type with enum
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", compositeName, " as (size ", enumName, ", count int4)"]))
                mempty
                Decoders.noResult
                True
            -- Test roundtrip
            Session.statement ("large", 5 :: Int32)
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select $1 :: ", compositeName]))
                ( Encoders.param
                    ( Encoders.nonNullable
                        ( Encoders.namedComposite
                            Nothing
                            compositeName
                            ( divide
                                (\(a, b) -> (a, b))
                                (Encoders.field (Encoders.nonNullable (Encoders.unnamedEnum id)))
                                (Encoders.field (Encoders.nonNullable Encoders.int4))
                            )
                        )
                    )
                )
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            ( Decoders.namedComposite
                                Nothing
                                compositeName
                                ( (,)
                                    <$> Decoders.field (Decoders.nonNullable (Decoders.namedEnum Nothing enumName (Just . id)))
                                    <*> Decoders.field (Decoders.nonNullable Decoders.int4)
                                )
                            )
                        )
                    )
                )
                True
          result `shouldBe` Right ("large", 5 :: Int32)

    describe "Arrays of enums" do
      it "encodes arrays of unnamed enums with explicit type cast" \config -> do
        enumName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create enum type
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", enumName, " as enum ('low', 'medium', 'high')"]))
                mempty
                Decoders.noResult
                True
            -- Test array encoding with explicit cast
            Session.statement ["low", "high", "medium"]
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select ($1 :: ", enumName, "[]) = array['low', 'high', 'medium'] :: ", enumName, "[]"]))
                ( Encoders.param
                    ( Encoders.nonNullable
                        ( Encoders.array
                            ( Encoders.dimension
                                foldl'
                                (Encoders.element (Encoders.nonNullable (Encoders.unnamedEnum id)))
                            )
                        )
                    )
                )
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
                True
          result `shouldBe` Right True

      it "roundtrips arrays of unnamed enums" \config -> do
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
            -- Test roundtrip with explicit cast
            Session.statement ["first", "third", "second"]
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select $1 :: ", enumName, "[]"]))
                ( Encoders.param
                    ( Encoders.nonNullable
                        ( Encoders.array
                            ( Encoders.dimension
                                foldl'
                                (Encoders.element (Encoders.nonNullable (Encoders.unnamedEnum id)))
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
          result `shouldBe` Right ["first", "third", "second"]
