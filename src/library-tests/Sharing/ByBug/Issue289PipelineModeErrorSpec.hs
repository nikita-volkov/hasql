module Sharing.ByBug.Issue289PipelineModeErrorSpec (spec) where

{- |
This module contains tests for issue #289: "Connection error: cannot enter pipeline mode, connection not idle"

The issue was reported when running multiple sessions sequentially using mapM with Connection.use.
The user was running migrations that were converted from Transaction types to Session types.

The error occurred after the change in version 1.10 where sessions now have exclusive access
to the connection for their entire duration (previously releasing and reacquiring the lock between statements).

These tests verify that:
1. Multiple sequential sessions using mapM work correctly
2. Alternating between script and statement sessions works
3. Explicit pipeline mode sessions can be run sequentially
4. Mixed script and pipeline sessions work together

All tests currently pass, which suggests either:
- The issue has been resolved
- The issue requires a very specific scenario not covered by these tests
- The issue is related to external factors (specific PostgreSQL version, hasql-transaction library, etc.)

These tests serve as regression tests to ensure this functionality continues to work correctly.
-}

import Data.Either
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Test.Hspec
import Prelude

spec :: SpecWith (Text, Word16)
spec = do
  describe "Issue #289" do
    it "Multiple sequential sessions with mapM should not cause 'cannot enter pipeline mode, connection not idle' error" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let simpleStatement =
              Statement.preparable
                "SELECT 1"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))

            createSession =
              Session.statement () simpleStatement

        -- Run multiple sessions sequentially using mapM
        -- This should reproduce the bug where the connection is not properly
        -- exiting pipeline mode between sessions
        results <- mapM (Connection.use connection) (replicate 3 createSession)

        -- All results should succeed
        all isRight results `shouldBe` True
        results `shouldBe` [Right 1, Right 1, Right 1]

    it "Multiple sequential sessions with script followed by statement should not cause pipeline mode error" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let simpleStatement =
              Statement.preparable
                "SELECT 1"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))

            -- Session that uses script (similar to migration pattern)
            scriptSession =
              Session.script "CREATE TABLE IF NOT EXISTS test_table (id INT)"

            -- Session that uses statement
            statementSession =
              Session.statement () simpleStatement

        -- Run multiple sessions sequentially: script, statement, script, statement
        -- This mimics the pattern from the issue where migrations are run
        result1 <- Connection.use connection scriptSession
        result2 <- Connection.use connection statementSession
        result3 <- Connection.use connection scriptSession
        result4 <- Connection.use connection statementSession

        -- All results should succeed
        result1 `shouldBe` Right ()
        result2 `shouldBe` Right 1
        result3 `shouldBe` Right ()
        result4 `shouldBe` Right 1

        -- Cleanup
        _ <- Connection.use connection (Session.script "DROP TABLE IF EXISTS test_table")
        pure ()

    it "Multiple sequential sessions using explicit pipeline mode should not cause 'cannot enter pipeline mode, connection not idle' error" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let simpleStatement =
              Statement.preparable
                "SELECT 1"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))

            -- Session that explicitly uses pipeline
            pipelineSession =
              Session.pipeline do
                Pipeline.statement () simpleStatement

        -- Run multiple pipeline sessions sequentially
        -- This should reproduce the issue where pipeline mode is not properly exited
        results <- mapM (Connection.use connection) (replicate 3 pipelineSession)

        -- All results should succeed
        all isRight results `shouldBe` True
        results `shouldBe` [Right 1, Right 1, Right 1]

    it "Alternating between script and pipeline sessions should work correctly" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let simpleStatement =
              Statement.preparable
                "SELECT 1"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))

            -- Session with script
            scriptSession =
              Session.script "SELECT 1"

            -- Session with explicit pipeline
            pipelineSession =
              Session.pipeline do
                Pipeline.statement () simpleStatement

        -- Alternate between script and pipeline sessions
        result1 <- Connection.use connection scriptSession
        result2 <- Connection.use connection pipelineSession
        result3 <- Connection.use connection scriptSession
        result4 <- Connection.use connection pipelineSession

        result1 `shouldBe` Right ()
        result2 `shouldBe` Right 1
        result3 `shouldBe` Right ()
        result4 `shouldBe` Right 1

