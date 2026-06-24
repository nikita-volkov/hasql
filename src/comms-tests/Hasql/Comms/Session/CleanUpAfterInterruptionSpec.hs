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
