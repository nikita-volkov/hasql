module Integration.Sharing.Session.StatementSpec (spec) where

import Data.Either
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Errors qualified as Errors
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Prelude
import Test.Hspec

spec :: SpecWith Scripts.ScopeParams
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
          Left (Errors.SessionUseError (Errors.StatementSessionError _ _ _ _ _ (Errors.ServerStatementError _))) -> pure ()
          _ -> expectationFailure $ "Unexpected result: " <> show result

    it "reports a statement that genuinely returned no rows as a row-count mismatch, not a lost connection" \config -> do
      -- The server answering with zero rows and the socket going away both
      -- reach `use` through the same absence of a result, and only an
      -- internal flag tells them apart (see `Hasql.Comms.Recv.NoResultsError`).
      -- Getting this wrong either way is a real hazard: reporting a genuine
      -- empty result as `ConnectionUseError` would have pools discard a
      -- perfectly good connection, and reporting a dropped socket as this
      -- recoverable mismatch would have callers keep using a connection that
      -- no longer exists (see "Integration.Sharing.Connection.Use.InterruptionSpec"
      -- and "Integration.Sharing.Pipeline.Statement.SendFailureSpec" for the
      -- latter).
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection (Session.statement () noRowsStatement)
        case result of
          Left (Errors.SessionUseError (Errors.StatementSessionError _ _ _ _ _ (Errors.UnexpectedRowCountStatementError 1 1 0))) -> pure ()
          _ -> expectationFailure ("Unexpected result: " <> show result)

        -- The connection is still fine afterwards.
        followUp <- Connection.use connection (Session.statement (1 :: Int64) echoStatement)
        followUp `shouldBe` Right 1

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

-- | Never returns a row, so a `Decoders.singleRow` decoder always sees zero
-- where it expects exactly one.
noRowsStatement :: Statement.Statement () Int64
noRowsStatement =
  Statement.preparable
    "select 1::int8 where false"
    mempty
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
