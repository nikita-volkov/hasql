module Hasql.EncodingDecodingSpec (spec) where

import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Hasql.TestingKit.Testcontainers qualified as Testcontainers
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic (assert, monadicIO, pre, run)
import Prelude hiding (assert)

spec :: Spec
spec = aroundAll Testcontainers.withConnection do
  describe "Encoding and Decoding" do
    describe "Array roundtrips" do
      it "handles 1D arrays" \connection -> property $ \(values :: [Int64]) -> monadicIO $ do
        let statement =
              Statement.Statement
                "select $1"
                (Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable Encoders.int8))))))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.array (Decoders.dimension replicateM (Decoders.element (Decoders.nonNullable Decoders.int8)))))))
                True
        result <- run $ Session.run (Session.statement values statement) connection
        assert $ result == Right values

      it "handles 2D arrays" \connection -> property $ \(values :: [Int64]) -> monadicIO $ do
        pre (not (null values))
        let input = replicate 3 values
        let statement =
              Statement.Statement
                "select $1"
                (Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable Encoders.int8)))))))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.array (Decoders.dimension replicateM (Decoders.dimension replicateM (Decoders.element (Decoders.nonNullable Decoders.int8))))))))
                True
        result <- run $ Session.run (Session.statement input statement) connection
        assert $ result == Right input

    describe "Composite types" do
      it "decodes simple composites" \connection -> do
        let statement =
              Statement.Statement
                "select (1, true)"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.composite ((,) <$> (Decoders.field (Decoders.nonNullable Decoders.int8)) <*> (Decoders.field (Decoders.nonNullable Decoders.bool)))))))
                True
        result <- Session.run (Session.statement () statement) connection
        result `shouldBe` Right (1 :: Int64, True)

      it "decodes complex composites" \connection -> do
        let statement =
              Statement.Statement
                "select ((1, true), ('hello', 3))"
                mempty
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            ( Decoders.composite
                                ( (,)
                                    <$> ( Decoders.field
                                            ( Decoders.nonNullable
                                                ( Decoders.composite
                                                    ( (,)
                                                        <$> (Decoders.field (Decoders.nonNullable Decoders.int8))
                                                        <*> (Decoders.field (Decoders.nonNullable Decoders.bool))
                                                    )
                                                )
                                            )
                                        )
                                    <*> ( Decoders.field
                                            ( Decoders.nonNullable
                                                ( Decoders.composite
                                                    ( (,)
                                                        <$> (Decoders.field (Decoders.nonNullable Decoders.text))
                                                        <*> (Decoders.field (Decoders.nonNullable Decoders.int8))
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
        result <- Session.run (Session.statement () statement) connection
        result `shouldBe` Right ((1 :: Int64, True), ("hello", 3 :: Int64))

    describe "Enum types" do
      it "handles enum encoding and decoding" \connection -> do
        -- First create the enum type
        let dropStatement = Statement.Statement "drop type if exists mood" mempty Decoders.noResult True
        let createStatement = Statement.Statement "create type mood as enum ('sad', 'ok', 'happy')" mempty Decoders.noResult True
        let testStatement =
              Statement.Statement
                "select ($1 :: mood)"
                (Encoders.param (Encoders.nonNullable (Encoders.enum id)))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.enum (Just . id)))))
                True

        result <-
          Session.run
            ( do
                Session.statement () dropStatement
                Session.statement () createStatement
                Session.statement "ok" testStatement
            )
            connection
        result `shouldBe` Right "ok"

    describe "Unknown enum" do
      it "handles unknown enum encoding" \connection -> do
        -- First create the enum type
        let dropStatement = Statement.Statement "drop type if exists mood" mempty Decoders.noResult True
        let createStatement = Statement.Statement "create type mood as enum ('sad', 'ok', 'happy')" mempty Decoders.noResult True
        let testStatement =
              Statement.Statement
                "select $1"
                (Encoders.param (Encoders.nonNullable (Encoders.unknownEnum id)))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.enum (Just . id)))))
                True

        result <-
          Session.run
            ( do
                Session.statement () dropStatement
                Session.statement () createStatement
                Session.statement "ok" testStatement
            )
            connection
        result `shouldBe` Right "ok"

    describe "Name-based types" do
      it "compiles enumByName functions" \_ -> do
        -- Test that the new functions compile and have correct types
        let encoder = Encoders.enumByName "test_type" id
        let decoder = Decoders.enumByName "test_type" Just
        -- Just test that these type-check, not that they execute
        seq encoder (seq decoder True) `shouldBe` True
        
      it "compiles compositeByName functions" \_ -> do
        -- Test that the new composite functions compile and have correct types  
        let compositeEnc = Encoders.field (Encoders.nonNullable Encoders.text)
        let encoder = Encoders.compositeByName "test_composite" compositeEnc
        let compositeDec = Decoders.field (Decoders.nonNullable Decoders.text)
        let decoder = Decoders.compositeByName "test_composite" compositeDec
        -- Just test that these type-check, not that they execute
        seq encoder (seq decoder True) `shouldBe` True
