module Isolated.Bugs.ExceptionConnectionResetRaceSpec (spec) where

import Control.Concurrent
import Control.Exception
import Data.IORef
import Hasql.Connection qualified as Connection
import Hasql.Session qualified as Session
import Helpers.Dsls.Execution qualified as Execution
import Helpers.Statements.SelectProvidedInt8 qualified as Statements
import System.Timeout
import Test.Hspec
import TestingKit.Testcontainers qualified as Testcontainers
import Prelude

spec :: Spec
spec = Testcontainers.aroundSpecWithConnection True do
  describe "Exception during session with concurrent access" do
    it "Connection remains usable after exception in non-idle state with concurrent threads" \connection -> do
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
          result <- Connection.use connection (Execution.sessionByParams (Statements.SelectProvidedInt8 42))
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
          finalResult <- Connection.use connection (Execution.sessionByParams (Statements.SelectProvidedInt8 99))
          finalResult `shouldBe` Right 99
