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
        result <- Connection.use connection (Session.statement (42 :: Int64) echoStatement)
        result `shouldBe` Right 42

    it "reuses a prepared statement across executions in one session" \config -> do
      -- The first execution is a cache miss (separate PARSE roundtrip),
      -- the second a cache hit (single roundtrip). Both must succeed.
      Scripts.onPreparableConnection config \connection -> do
        result <-
          Connection.use connection do
            a <- Session.statement (1 :: Int64) echoStatement
            b <- Session.statement (2 :: Int64) echoStatement
            pure (a, b)
        result `shouldBe` Right (1, 2)

    it "keeps a prepared statement usable after an EXECUTE error" \config -> do
      -- Regression: PARSE succeeds, EXECUTE fails (division by zero). The
      -- statement is on the server under its cached name, so a later use on the
      -- same connection must hit the cache rather than re-issuing PARSE for an
      -- already-existing name ("prepared statement ... already exists").
      Scripts.onPreparableConnection config \connection -> do
        failure <- Connection.use connection (Session.statement 0 divStatement)
        failure `shouldSatisfy` isLeft
        success <- Connection.use connection (Session.statement 1 divStatement)
        success `shouldBe` Right 1

    it "works on an unpreparable connection" \config -> do
      Scripts.onUnpreparableConnection config \connection -> do
        result <- Connection.use connection (Session.statement (42 :: Int64) echoStatement)
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
          Left (Errors.StatementSessionError _ _ _ _ _ (Errors.ServerStatementError _)) -> pure ()
          _ -> expectationFailure $ "Unexpected result: " <> show result

echoStatement :: Statement.Statement Int64 Int64
echoStatement =
  Statement.preparable
    "select $1"
    (Encoders.param (Encoders.nonNullable Encoders.int8))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))

-- | Parses fine, but fails at execution time when given 0 (division by zero).
divStatement :: Statement.Statement Int64 Int64
divStatement =
  Statement.preparable
    "select 1 / $1"
    (Encoders.param (Encoders.nonNullable Encoders.int8))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
