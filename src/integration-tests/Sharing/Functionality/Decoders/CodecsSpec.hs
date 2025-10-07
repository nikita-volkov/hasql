module Sharing.Functionality.Decoders.CodecsSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic (assert, monadicIO, pre, run)
import Prelude hiding (assert)

spec :: SpecWith (Text, Word16)
spec = do
  describe "Miscellaneous Tests" do
    describe "Interval types" do
      it "encodes intervals correctly" \config -> Scripts.onConnection config \connection -> do
        let statement =
              Statement.Statement
                "select $1 = interval '10 seconds'"
                (Encoders.param (Encoders.nonNullable Encoders.interval))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
                True
        result <- Connection.use connection (Session.statement (10 :: DiffTime) statement)
        result `shouldBe` Right True

      it "decodes intervals correctly" \config -> Scripts.onConnection config \connection -> do
        let statement =
              Statement.Statement
                "select interval '10 seconds'"
                Encoders.noParams
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.interval)))
                True
        result <- Connection.use connection (Session.statement () statement)
        result `shouldBe` Right (10 :: DiffTime)

      it "roundtrips intervals correctly" \config -> Scripts.onConnection config \connection -> do
        let statement =
              Statement.Statement
                "select $1"
                (Encoders.param (Encoders.nonNullable Encoders.interval))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.interval)))
                True
        result <- Connection.use connection (Session.statement (10 :: DiffTime) statement)
        result `shouldBe` Right (10 :: DiffTime)

  describe "Encoding and Decoding" do
    describe "Array roundtrips" do
      it "handles 1D arrays" \config -> Scripts.onConnection config \connection -> property $ \(values :: [Int64]) -> monadicIO $ do
        let statement =
              Statement.Statement
                "select $1"
                (Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable Encoders.int8))))))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.array (Decoders.dimension replicateM (Decoders.element (Decoders.nonNullable Decoders.int8)))))))
                True
        result <- run $ Connection.use connection (Session.statement values statement)
        assert $ result == Right values

      it "handles 2D arrays" \config -> Scripts.onConnection config \connection -> property $ \(values :: [Int64]) -> monadicIO $ do
        pre (not (null values))
        let input = replicate 3 values
        let statement =
              Statement.Statement
                "select $1"
                (Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable Encoders.int8)))))))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.array (Decoders.dimension replicateM (Decoders.dimension replicateM (Decoders.element (Decoders.nonNullable Decoders.int8))))))))
                True
        result <- run $ Connection.use connection (Session.statement input statement)
        assert $ result == Right input

    describe "Composite types" do
      it "decodes simple composites" \config -> Scripts.onConnection config \connection -> do
        let statement =
              Statement.Statement
                "select (1, true)"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.composite ((,) <$> (Decoders.field (Decoders.nonNullable Decoders.int8)) <*> (Decoders.field (Decoders.nonNullable Decoders.bool)))))))
                True
        result <- Connection.use connection (Session.statement () statement)
        result `shouldBe` Right (1 :: Int64, True)

      it "decodes complex composites" \config -> Scripts.onConnection config \connection -> do
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
        result <- Connection.use connection (Session.statement () statement)
        result `shouldBe` Right ((1 :: Int64, True), ("hello", 3 :: Int64))
