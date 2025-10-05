module Features.SessionSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec
import Test.QuickCheck.Instances ()
import TestingKit.Testcontainers qualified as Testcontainers
import Prelude

spec :: Spec
spec = Testcontainers.aroundSpecWithConnection False do
  describe "Basic Session Operations" do
    describe "Roundtrips" do
      it "handles simple values correctly" \connection -> do
        let statement =
              Statement.Statement
                "select $1"
                (Encoders.param (Encoders.nonNullable Encoders.int8))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
                True
        result <- Connection.use connection (Session.statement (42 :: Int64) statement)
        result `shouldBe` Right 42

    describe "Error Handling" do
      it "captures query errors correctly" \connection -> do
        let statement =
              Statement.Statement
                "select true where 1 = any ($1) and $2"
                ( mconcat
                    [ fst >$< (Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable Encoders.int8)))))),
                      snd >$< (Encoders.param (Encoders.nonNullable Encoders.text))
                    ]
                )
                (fmap (maybe False (const True)) (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.bool))))
                True
        result <- Connection.use connection (Session.statement ([3, 7] :: [Int64], "a") statement)
        case result of
          Left (Connection.ServerUsageError {}) -> pure ()
          _ -> expectationFailure $ "Unexpected result: " <> show result

    describe "IN simulation" do
      it "works with arrays" \connection -> do
        let statement =
              Statement.Statement
                "select true where 1 = any ($1)"
                (Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable Encoders.int8))))))
                (fmap (maybe False (const True)) (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.bool))))
                True
        result <- Connection.use connection do
          result1 <- Session.statement ([1, 2] :: [Int64]) statement
          result2 <- Session.statement ([2, 3] :: [Int64]) statement
          return (result1, result2)
        result `shouldBe` Right (True, False)

    describe "NOT IN simulation" do
      it "works with arrays" \connection -> do
        let statement =
              Statement.Statement
                "select true where 3 <> all ($1)"
                (Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable Encoders.int8))))))
                (fmap (maybe False (const True)) (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.bool))))
                True
        result <- Connection.use connection do
          result1 <- Session.statement ([1, 2] :: [Int64]) statement
          result2 <- Session.statement ([2, 3] :: [Int64]) statement
          return (result1, result2)
        result `shouldBe` Right (True, False)
