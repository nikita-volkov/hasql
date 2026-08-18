module Integration.Sharing.Connection.UseSpec (spec) where

import Control.Concurrent
import Control.Exception
import Data.Either
import Data.IORef
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Dsls.Execution qualified as Execution
import Helpers.Scripts qualified as Scripts
import Helpers.Statements.SelectOne qualified as Statements.SelectOne
import Helpers.Statements.SelectProvidedInt8 qualified as Statements.SelectProvidedInt8
import Helpers.Statements.Sleep qualified as Statements
import Prelude
import Test.Hspec

spec :: SpecWith Scripts.ScopeParams
spec = do
  describe "Transactions" do
    it "Do not cause \"in progress after error\"" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let sumStatement =
              Statement.preparable
                "select ($1 + $2)"
                ( mconcat
                    [ fst >$< Encoders.param (Encoders.nonNullable Encoders.int8),
                      snd >$< Encoders.param (Encoders.nonNullable Encoders.int8)
                    ]
                )
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))

        result <-
          Connection.use connection do
            Session.script "."

        result `shouldSatisfy` isLeft

        result <-
          Connection.use connection do
            Session.script "begin;"
            s <- Session.statement (1 :: Int64, 2 :: Int64) sumStatement
            Session.script "end;"
            return s

        result `shouldBe` Right (3 :: Int64)

  describe "Pipeline Mode" do
    it "Leaves the connection usable after timeout in pipeline" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let selectStatement =
              Statement.preparable
                "select $1::int"
                (Encoders.param (Encoders.nonNullable Encoders.int4))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))

        -- Timeout during a pipeline operation
        result <-
          timeout 50_000 do
            Connection.use connection
              $ Session.pipeline
              $ (,)
              <$> Pipeline.statement 42 selectStatement
              <*> Execution.pipelineByParams (Statements.Sleep 0.1)

        result `shouldBe` Nothing

        -- Try to use pipeline again after timeout cleanup
        -- This should work but fails with "connection not idle" without the fix
        result2 <-
          Connection.use connection
            $ Session.pipeline
            $ Pipeline.statement 99 selectStatement

        result2 `shouldBe` Right 99

  describe "Timing out" do
    describe "On a statement" do
      it "Leaves the connection usable" \config -> Scripts.onPreparableConnection config \connection -> do
        result <-
          timeout 50_000 do
            Connection.use connection do
              Execution.sessionByParams (Statements.Sleep 0.1)

        result `shouldBe` Nothing

        result <-
          Connection.use connection do
            Execution.sessionByParams Statements.SelectOne.SelectOne

        result `shouldBe` Right 1

    describe "On a transaction" do
      it "Leaves the connection usable" \config -> Scripts.onPreparableConnection config \connection -> do
        -- Start a transaction and timeout during it
        result <-
          timeout 50_000 do
            Connection.use connection do
              Session.script "begin;"
              Execution.sessionByParams (Statements.Sleep 0.1)
              Session.script "commit;"

        result `shouldBe` Nothing

        -- Connection should still be usable after timeout in transaction
        result <-
          Connection.use connection do
            Execution.sessionByParams Statements.SelectOne.SelectOne

        result `shouldBe` Right 1

      it "Lets us start another transaction" do
        let checkTransactionStatus =
              Statement.preparable
                "select case when pg_advisory_lock(1) is null then 0 else 1 end"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
         in \config -> Scripts.onPreparableConnection config \connection -> do
              -- Timeout during a transaction
              result <-
                timeout 50_000 do
                  Connection.use connection do
                    Session.script "begin;"
                    Execution.sessionByParams (Statements.Sleep 0.1)

              result `shouldBe` Nothing

              -- Verify we can start a new transaction without "already in progress" error
              result <-
                Connection.use connection do
                  Session.script "begin;"
                  s <- Session.statement () checkTransactionStatus
                  Session.script "commit;"
                  return s

              result `shouldBe` Right 1

      it "Does not corrupt the prepared statement registry" do
        let returnIntStatement =
              Statement.preparable
                "select $1::int"
                (Encoders.param (Encoders.nonNullable Encoders.int4))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
         in \config -> Scripts.onPreparableConnection config \connection -> do
              -- Use a prepared statement first
              result <-
                Connection.use connection do
                  Session.statement 42 returnIntStatement

              result `shouldBe` Right 42

              -- Timeout during transaction (causes connection reset)
              result <-
                timeout 50_000 do
                  Connection.use connection do
                    Session.script "begin;"
                    Execution.sessionByParams (Statements.Sleep 0.1)
                    Session.script "commit;"

              result `shouldBe` Nothing

              -- The prepared statement should work again without "does not exist" error
              result <-
                Connection.use connection do
                  Session.statement 99 returnIntStatement

              result `shouldBe` Right 99

  describe "Interruption by an exception" do
    it "Re-throws the original exception rather than a driver wrapper around it" \config -> Scripts.onPreparableConnection config \connection -> do
      -- The state a session got as far as reaches `use` on the back of an
      -- exception, so `use` has to unwrap that carrier before re-throwing.
      -- Caught here at the concrete type the session threw: were the carrier
      -- to escape instead, `try` would not match it and it would fail the
      -- example by propagating.
      result <- try @IOException do
        Connection.use connection do
          Session.script "select 1"
          liftIO (throwIO (userError "Original exception"))

      case result of
        Left err -> show err `shouldContain` "Original exception"
        Right _ -> expectationFailure "Expected the exception to propagate out of `use`"

  describe "Concurrency" do
    it "handles concurrent connections properly" \config -> do
      Scripts.onPreparableConnection config \connection1 -> do
        Scripts.onPreparableConnection config \connection2 -> do
          let selectSleep =
                Statement.preparable
                  "select pg_sleep($1)"
                  (Encoders.param (Encoders.nonNullable Encoders.float8))
                  Decoders.noResult

          beginVar <- newEmptyMVar
          finishVar <- newEmptyMVar

          _ <- forkIO do
            putMVar beginVar ()
            _ <- Connection.use connection1 (Session.statement (0.2 :: Double) selectSleep)
            void (tryPutMVar finishVar False)

          _ <- forkIO do
            takeMVar beginVar
            _ <- Connection.use connection2 (Session.statement (0.1 :: Double) selectSleep)
            void (tryPutMVar finishVar True)

          -- The second connection should finish first (True)
          result <- takeMVar finishVar
          result `shouldBe` True

    it "Connection remains usable after exception in non-idle state with concurrent threads" \config -> Scripts.onPreparableConnection config \connection -> do
      -- This test reproduces the bug fixed in commit 62ebef2.
      -- The bug was that when an exception occurred during a session,
      -- the connection state was put back into the MVar BEFORE resetting the connection.
      -- This created a race condition where another thread could grab the corrupted connection.

      -- We'll create a scenario where:
      -- 1. Thread A starts a session that will throw an exception
      -- 2. Thread B repeatedly tries to use the connection
      -- 3. The exception in Thread A should not corrupt the connection for Thread B

      -- Counter to track successful operations by Thread B
      successCount <- newIORef (0 :: Int)
      errorCount <- newIORef (0 :: Int)

      -- Barrier to synchronize threads
      startBarrier <- newEmptyMVar
      doneBarrier <- newEmptyMVar

      -- Thread A: Throws exceptions repeatedly
      _ <- forkIO do
        takeMVar startBarrier
        replicateM_ 10 do
          -- Use the connection and throw an exception during the session
          _ <- try @SomeException do
            Connection.use connection do
              -- Start a transaction to put connection in non-idle state
              Session.script "BEGIN"
              -- Throw an exception while in transaction (non-idle state)
              liftIO (throwIO (userError "Intentional exception"))
          threadDelay 1000 -- Small delay to allow interleaving
        putMVar doneBarrier ()

      -- Thread B: Tries to use connection concurrently
      _ <- forkIO do
        takeMVar startBarrier
        replicateM_ 20 do
          result <- Connection.use connection (Execution.sessionByParams (Statements.SelectProvidedInt8.SelectProvidedInt8 42))
          case result of
            Right 42 -> atomicModifyIORef' successCount (\n -> (n + 1, ()))
            _ -> atomicModifyIORef' errorCount (\n -> (n + 1, ()))
          threadDelay 500
        putMVar doneBarrier ()

      -- Start both threads
      putMVar startBarrier ()
      putMVar startBarrier ()

      -- Wait for both threads to complete with a timeout
      -- If the bug exists, threads may hang waiting for a corrupted connection
      result <- timeout (5 * 1000000) do
        -- 5 seconds timeout
        takeMVar doneBarrier
        takeMVar doneBarrier

      case result of
        Nothing -> do
          -- Test timed out - this indicates the bug is present
          expectationFailure "Test timed out waiting for threads to complete. This indicates the connection became deadlocked due to the race condition bug."
        Just () -> do
          -- Threads completed successfully
          -- Check results
          successes <- readIORef successCount
          errors <- readIORef errorCount

          -- Thread B should have succeeded at least some times
          -- If the bug exists, we'd expect Thread B to get errors due to corrupted connection state
          successes `shouldSatisfy` (> 0)

          errors `shouldBe` 0

          -- Verify connection is still usable after all this
          finalResult <- Connection.use connection (Execution.sessionByParams (Statements.SelectProvidedInt8.SelectProvidedInt8 99))
          finalResult `shouldBe` Right 99
