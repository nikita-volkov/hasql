module Features.DecoderCompatibilityCheckSpec (spec) where

import Data.Either
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec
import TestingKit.Testcontainers qualified as Testcontainers
import Prelude

spec :: Spec
spec = aroundAll Testcontainers.withConnection do
  describe "Error Handling" do
    it "reports UnexpectedAmountOfColumns when result has more columns" \connection -> do
      let statement =
            Statement.Statement
              "select 1, 2"
              mempty
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
              True
      result <- Connection.use connection (Session.statement () statement)
      case result of
        Left (Session.QueryError _ _ (Session.ResultError (Session.UnexpectedAmountOfColumns expected actual))) -> do
          shouldBe expected 1
          shouldBe actual 2
        Left err ->
          expectationFailure ("Unexpected type of error: " <> show err)
        result ->
          expectationFailure ("Not an error: " <> show result)

    it "reports UnexpectedAmountOfColumns when result has fewer columns" \connection -> do
      let statement =
            Statement.Statement
              "select 1"
              mempty
              ( Decoders.singleRow
                  ( (,)
                      <$> Decoders.column (Decoders.nonNullable Decoders.int8)
                      <*> Decoders.column (Decoders.nonNullable Decoders.int8)
                  )
              )
              True
      result <- Connection.use connection (Session.statement () statement)
      case result of
        Left (Session.QueryError _ _ (Session.ResultError (Session.UnexpectedAmountOfColumns expected actual))) -> do
          shouldBe expected 2
          shouldBe actual 1
        Left err ->
          expectationFailure ("Unexpected type of error: " <> show err)
        result ->
          expectationFailure ("Not an error: " <> show result)

    it "reports DecoderTypeMismatch when column type mismatches decoder" \connection -> do
      let statement =
            Statement.Statement
              "select 1, 'text'"
              mempty
              ( Decoders.singleRow
                  ( (,)
                      <$> Decoders.column (Decoders.nonNullable Decoders.int8)
                      <*> Decoders.column (Decoders.nonNullable Decoders.int8)
                  )
              )
              True
      result <- Connection.use connection (Session.statement () statement)
      case result of
        Left (Session.QueryError _ _ (Session.ResultError (Session.DecoderTypeMismatch column expected actual))) -> do
          shouldBe column 1
          shouldBe expected 20
          shouldBe actual 25
        Left err ->
          expectationFailure ("Unexpected type of error: " <> show err)
        result ->
          expectationFailure ("Not an error: " <> show result)
