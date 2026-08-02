module Hasql.Comms.Session.CleanUpAfterInterruptionSpec (spec) where

import Hasql.Comms.Session qualified as Session
import Hasql.Comms.SpecHook (ConnectWith (..))
import Hasql.Platform.Prelude
import Pqi qualified
import Test.Hspec
import TextBuilder qualified

spec :: SpecWith (ConnectWith, Text, Word16)
spec = do
  describe "cleanUpAfterInterruption" do
    it "cleans up when in pipeline mode" \config -> do
      withConnection config \connection -> do
        -- Enter pipeline mode
        success <- Pqi.enterPipelineMode connection
        success `shouldBe` True

        -- Verify we're in pipeline mode
        status <- Pqi.pipelineStatus connection
        status `shouldBe` Pqi.PipelineOn

        -- Run cleanup
        result <- Session.toHandler Session.cleanUpAfterInterruption connection
        result `shouldBe` Right ()

        -- Verify we're out of pipeline mode
        status <- Pqi.pipelineStatus connection
        status `shouldBe` Pqi.PipelineOff

    it "cleans up when the pipeline is aborted with no pending transaction or sync" \config -> do
      withConnection config \connection -> do
        success <- Pqi.enterPipelineMode connection
        success `shouldBe` True

        sent <- Pqi.sendQueryParams connection "SELECT * FROM nonexistent_table" [] Pqi.Text
        sent `shouldBe` True
        flushRequestSent <- Pqi.sendFlushRequest connection
        flushRequestSent `shouldBe` True
        flushStatus <- Pqi.flush connection
        flushStatus `shouldBe` Pqi.FlushOk

        let waitForResult attempts = do
              when (attempts <= 0) do
                expectationFailure "Timed out waiting for libpq result after flushing pipeline query"
              consumed <- Pqi.consumeInput connection
              consumed `shouldBe` True
              busy <- Pqi.isBusy connection
              if busy
                then threadDelay 1000 >> waitForResult (attempts - 1)
                else Pqi.getResult connection

        result <- waitForResult (10000 :: Int)
        resultStatus <- traverse Pqi.resultStatus result
        resultStatus `shouldBe` Just Pqi.FatalError
        pipelineStatus <- Pqi.pipelineStatus connection
        pipelineStatus `shouldBe` Pqi.PipelineAborted

        cleanupResult <- Session.toHandler Session.cleanUpAfterInterruption connection
        cleanupResult `shouldBe` Right ()
        finalPipelineStatus <- Pqi.pipelineStatus connection
        finalPipelineStatus `shouldBe` Pqi.PipelineOff

    -- Deterministic counterpart to the timing-based reproduction in
    -- "Sharing.ByBug.PipelineAbortedInterruptionCleanupSpec": instead of
    -- racing an async exception into the narrow window during which libpq's
    -- pipeline is in the aborted state, we put the connection into that state
    -- directly and invoke the cleanup that an interruption would have
    -- invoked. This exercises the `leavePipeline` branch that used to only
    -- handle `PipelineOn`, leaving an aborted pipeline to fall through to
    -- `bringTransactionStatusToIdle`, which then tried to send "ABORT" as a
    -- serial command while still in pipeline mode.
    it "cleans up when the pipeline is aborted" \config -> do
      withConnection config \connection -> do
        -- Open a transaction, so that the failing statement below leaves the
        -- connection in the error transaction status -- the status that makes
        -- `bringTransactionStatusToIdle` want to send "ABORT" as a serial
        -- command, which libpq refuses while pipeline mode is still on
        _ <- Pqi.exec connection "BEGIN"

        -- Enter pipeline mode
        success <- Pqi.enterPipelineMode connection
        success `shouldBe` True

        -- Queue a statement that is guaranteed to fail on the server,
        -- followed by another one. The trailing statement matters: it makes
        -- the abort survive the drains that `cleanUpAfterInterruption`
        -- performs before reaching `leavePipeline`, since `drainResults`
        -- stops at each command boundary
        success <- Pqi.sendQueryParams connection "select 1/0" [] Pqi.Text
        success `shouldBe` True

        success <- Pqi.sendQueryParams connection "select 1" [] Pqi.Text
        success `shouldBe` True

        -- Flush the queued statements to the server
        success <- Pqi.pipelineSync connection
        success `shouldBe` True

        -- Consume results until libpq registers the error and flips the
        -- pipeline into the aborted state, leaving the trailing sync marker
        -- undrained -- exactly the state an interruption would leave behind
        consumeUntilPipelineAborted connection

        -- Run cleanup
        result <- Session.toHandler Session.cleanUpAfterInterruption connection
        result `shouldBe` Right ()

        -- Verify we're out of pipeline mode and usable again
        status <- Pqi.pipelineStatus connection
        status `shouldBe` Pqi.PipelineOff

        transStatus <- Pqi.transactionStatus connection
        transStatus `shouldBe` Pqi.TransIdle

    it "cleans up when in an open transaction" \config -> do
      withConnection config \connection -> do
        -- Start a transaction
        _ <- Pqi.exec connection "BEGIN"

        -- Verify we're in a transaction
        transStatus <- Pqi.transactionStatus connection
        transStatus `shouldBe` Pqi.TransInTrans

        -- Run cleanup
        result <- Session.toHandler Session.cleanUpAfterInterruption connection
        result `shouldBe` Right ()

        -- Verify transaction was aborted (idle state)
        transStatus <- Pqi.transactionStatus connection
        transStatus `shouldBe` Pqi.TransIdle

    it "cleans up when in error state within transaction" \config -> do
      withConnection config \connection -> do
        -- Start a transaction
        _ <- Pqi.exec connection "BEGIN"

        -- Cause an error
        _ <- Pqi.exec connection "SELECT * FROM nonexistent_table"

        -- Verify we're in error state
        transStatus <- Pqi.transactionStatus connection
        transStatus `shouldBe` Pqi.TransInError

        -- Run cleanup
        result <- Session.toHandler Session.cleanUpAfterInterruption connection
        result `shouldBe` Right ()

        -- Verify transaction was aborted (idle state)
        transStatus <- Pqi.transactionStatus connection
        transStatus `shouldBe` Pqi.TransIdle

    it "cleans up with pending results" \config -> do
      withConnection config \connection -> do
        -- Send a query without retrieving results
        success <- Pqi.sendQuery connection "SELECT 1"
        success `shouldBe` True

        -- Run cleanup (should drain results)
        result <- Session.toHandler Session.cleanUpAfterInterruption connection
        result `shouldBe` Right ()

        -- Verify connection is in idle state
        transStatus <- Pqi.transactionStatus connection
        transStatus `shouldBe` Pqi.TransIdle

    it "cleans up with prepared statements" \config -> do
      withConnection config \connection -> do
        -- Create a prepared statement
        mResult <- Pqi.prepare connection "test_stmt" "SELECT $1::int" Nothing
        case mResult of
          Just _ -> pure ()
          Nothing -> expectationFailure "Expected result from prepare"

        -- Consume any remaining results
        _ <- Pqi.getResult connection

        -- Verify the prepared statement exists by using it
        _ <- Pqi.execPrepared connection "test_stmt" [Just ("42", Pqi.Text)] Pqi.Text

        -- Run cleanup (should deallocate all statements)
        cleanupResult <- Session.toHandler Session.cleanUpAfterInterruption connection
        cleanupResult `shouldBe` Right ()

        -- Try to execute the prepared statement - it should fail now
        mResult2 <- Pqi.execPrepared connection "test_stmt" [Just ("42", Pqi.Text)] Pqi.Text
        case mResult2 of
          Just result -> do
            status <- Pqi.resultStatus result
            -- Should get an error status because the statement was deallocated
            status `shouldBe` Pqi.FatalError
          Nothing -> expectationFailure "Expected error result from exec prepared after deallocation"

-- * Helpers

-- | Pull results from a pipelined connection until libpq reports the pipeline
-- as aborted, failing the example if that never happens.
consumeUntilPipelineAborted :: (Pqi.IsConnection c) => c -> IO ()
consumeUntilPipelineAborted connection =
  go (10 :: Int)
  where
    go attemptsLeft = do
      status <- Pqi.pipelineStatus connection
      case status of
        Pqi.PipelineAborted -> pure ()
        _
          | attemptsLeft <= 0 ->
              expectationFailure
                ("Pipeline did not reach the aborted state. Last status: " <> show status)
          | otherwise -> do
              _ <- Pqi.getResult connection
              go (pred attemptsLeft)

withConnection ::
  (ConnectWith, Text, Word16) ->
  (forall c. (Pqi.IsConnection c) => c -> IO a) ->
  IO a
withConnection (ConnectWith connect, host, port) action =
  let connectionString =
        (encodeUtf8 . TextBuilder.toText . mconcat)
          [ "host=",
            TextBuilder.text host,
            " port=",
            TextBuilder.decimal port,
            " user=postgres",
            " password=postgres",
            " dbname=postgres"
          ]
   in bracket
        (connect connectionString)
        ( \connection -> do
            Pqi.finish connection
        )
        ( \connection -> do
            status <- Pqi.status connection
            case status of
              Pqi.ConnectionOk -> action connection
              _ -> do
                errorMessage <- Pqi.errorMessage connection
                fail ("Connection failed: " <> show errorMessage)
        )
