module Comms.Session.CleanUpAfterInterruptionSpec (spec) where

import Hasql.Comms.Session qualified as Session
import Hasql.Platform.Prelude
import Hasql.Pq qualified as Pq
import Test.Hspec
import TextBuilder qualified

spec :: SpecWith (Text, Word16)
spec = do
  describe "cleanUpAfterInterruption" do
    it "cleans up when in pipeline mode" \config -> do
      withConnection config \connection -> do
        -- Enter pipeline mode
        success <- Pq.enterPipelineMode connection
        success `shouldBe` True

        -- Verify we're in pipeline mode
        status <- Pq.pipelineStatus connection
        status `shouldBe` Pq.PipelineOn

        -- Run cleanup
        result <- Session.toHandler Session.cleanUpAfterInterruption connection
        result `shouldBe` Right ()

        -- Verify we're out of pipeline mode
        status <- Pq.pipelineStatus connection
        status `shouldBe` Pq.PipelineOff

    it "cleans up when in an open transaction" \config -> do
      withConnection config \connection -> do
        -- Start a transaction
        _ <- Pq.exec connection "BEGIN"

        -- Verify we're in a transaction
        transStatus <- Pq.transactionStatus connection
        transStatus `shouldBe` Pq.TransInTrans

        -- Run cleanup
        result <- Session.toHandler Session.cleanUpAfterInterruption connection
        result `shouldBe` Right ()

        -- Verify transaction was aborted (idle state)
        transStatus <- Pq.transactionStatus connection
        transStatus `shouldBe` Pq.TransIdle

    it "cleans up when in error state within transaction" \config -> do
      withConnection config \connection -> do
        -- Start a transaction
        _ <- Pq.exec connection "BEGIN"

        -- Cause an error
        _ <- Pq.exec connection "SELECT * FROM nonexistent_table"

        -- Verify we're in error state
        transStatus <- Pq.transactionStatus connection
        transStatus `shouldBe` Pq.TransInError

        -- Run cleanup
        result <- Session.toHandler Session.cleanUpAfterInterruption connection
        result `shouldBe` Right ()

        -- Verify transaction was aborted (idle state)
        transStatus <- Pq.transactionStatus connection
        transStatus `shouldBe` Pq.TransIdle

    it "cleans up with pending results" \config -> do
      withConnection config \connection -> do
        -- Send a query without retrieving results
        success <- Pq.sendQuery connection "SELECT 1"
        success `shouldBe` True

        -- Run cleanup (should drain results)
        result <- Session.toHandler Session.cleanUpAfterInterruption connection
        result `shouldBe` Right ()

        -- Verify connection is in idle state
        transStatus <- Pq.transactionStatus connection
        transStatus `shouldBe` Pq.TransIdle

    it "cleans up with prepared statements" \config -> do
      withConnection config \connection -> do
        -- Create a prepared statement
        mResult <- Pq.prepare connection "test_stmt" "SELECT $1::int" Nothing
        case mResult of
          Just _ -> pure ()
          Nothing -> expectationFailure "Expected result from prepare"

        -- Consume any remaining results
        _ <- Pq.getResult connection

        -- Verify the prepared statement exists by using it
        _ <- Pq.execPrepared connection "test_stmt" [Just ("42", Pq.Text)] Pq.Text

        -- Run cleanup (should deallocate all statements)
        cleanupResult <- Session.toHandler Session.cleanUpAfterInterruption connection
        cleanupResult `shouldBe` Right ()

        -- Try to execute the prepared statement - it should fail now
        mResult2 <- Pq.execPrepared connection "test_stmt" [Just ("42", Pq.Text)] Pq.Text
        case mResult2 of
          Just result -> do
            status <- Pq.resultStatus result
            -- Should get an error status because the statement was deallocated
            status `shouldBe` Pq.FatalError
          Nothing -> expectationFailure "Expected error result from exec prepared after deallocation"

-- * Helpers

withConnection :: (Text, Word16) -> (Pq.Connection -> IO a) -> IO a
withConnection (host, port) action =
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
        (Pq.connectdb connectionString)
        ( \connection -> do
            Pq.finish connection
        )
        ( \connection -> do
            status <- Pq.status connection
            case status of
              Pq.ConnectionOk -> action connection
              _ -> do
                errorMessage <- Pq.errorMessage connection
                fail ("Connection failed: " <> show errorMessage)
        )
