module Sharing.ByBug.Issue289PipelineModeErrorSpec (spec) where

import Data.Either
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
