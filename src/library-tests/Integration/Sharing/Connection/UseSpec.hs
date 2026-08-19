module Integration.Sharing.Connection.UseSpec (spec) where

import Control.Concurrent
import Control.Exception
import Data.Either
import Data.IORef
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Errors qualified as Errors
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Dsls.Execution qualified as Execution
import Helpers.Scripts qualified as Scripts
import Helpers.Statements.SelectProvidedInt8 qualified as Statements.SelectProvidedInt8
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

    it "Reports the connection as gone to every other thread rather than deadlocking" \config -> Scripts.onPreparableConnection config \connection -> do
      -- The exception path of `use` empties the MVar before it touches the
      -- connection and fills it back in with `Nothing`, so a thread waiting
      -- on the MVar wakes up to a handle that says the connection is gone.
      -- What it must never do is wake up to the connection itself while the
      -- interrupted thread is still finishing it, or not wake up at all -
      -- which is what happened before commit 62ebef2, where the state went
      -- back into the MVar before the connection was dealt with.
      --
      -- So this pins the two halves that survive the connection no longer
      -- being repaired: every result another thread sees is either the
      -- statement's own or the verdict that the connection is gone, and no
      -- thread hangs.

      -- Results collected by the thread using the connection alongside the
      -- one that interrupts it.
      resultsRef <- newIORef []

      startBarrier <- newEmptyMVar
      doneBarrier <- newEmptyMVar

      -- Thread A: interrupts a session that is in a transaction, so the
      -- connection is left in a non-idle state.
      _ <- forkIO do
        takeMVar startBarrier
        replicateM_ 10 do
          _ <- try @SomeException do
            Connection.use connection do
              Session.script "BEGIN"
              liftIO (throwIO (userError "Intentional exception"))
          threadDelay 1000
        putMVar doneBarrier ()

      -- Thread B: keeps using the connection throughout.
      _ <- forkIO do
        takeMVar startBarrier
        replicateM_ 20 do
          result <- Connection.use connection (Execution.sessionByParams (Statements.SelectProvidedInt8.SelectProvidedInt8 42))
          atomicModifyIORef' resultsRef (\results -> (result : results, ()))
          threadDelay 500
        putMVar doneBarrier ()

      putMVar startBarrier ()
      putMVar startBarrier ()

      completion <- timeout (5 * 1000000) do
        takeMVar doneBarrier
        takeMVar doneBarrier

      case completion of
        Nothing ->
          expectationFailure "Timed out waiting for the threads to complete, which means the connection deadlocked."
        Just () -> do
          results <- readIORef resultsRef
          let unexpected =
                filter
                  ( \case
                      Right 42 -> False
                      Left (Errors.ConnectionSessionError _) -> False
                      _ -> True
                  )
                  results
          unexpected `shouldBe` []

          -- Once gone, gone: the handle does not come back to life.
          finalResult <- Connection.use connection (Execution.sessionByParams (Statements.SelectProvidedInt8.SelectProvidedInt8 99))
          case finalResult of
            Left (Errors.ConnectionSessionError _) -> pure ()
            _ -> expectationFailure ("Unexpected final result: " <> show finalResult)
