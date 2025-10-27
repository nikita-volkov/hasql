module Sharing.ByUnit.Session.StatementSpec (spec) where

import Data.Either
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
  describe "Roundtrips" do
    it "handles simple values correctly" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.preparable
                "select $1"
                (Encoders.param (Encoders.nonNullable Encoders.int8))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
        result <- Connection.use connection (Session.statement (42 :: Int64) statement)
        result `shouldBe` Right 42

  describe "Error Handling" do
    it "captures query errors correctly" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.preparable
                "select true where 1 = any ($1) and $2"
                ( mconcat
                    [ fst >$< (Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable Encoders.int8)))))),
                      snd >$< (Encoders.param (Encoders.nonNullable Encoders.text))
                    ]
                )
                (fmap (maybe False (const True)) (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.bool))))
        result <- Connection.use connection (Session.statement ([3, 7] :: [Int64], "a") statement)
        case result of
          Left (Errors.StatementSessionError _ _ _ _ _ (Errors.ExecutionStatementError _)) -> pure ()
          _ -> expectationFailure $ "Unexpected result: " <> show result
