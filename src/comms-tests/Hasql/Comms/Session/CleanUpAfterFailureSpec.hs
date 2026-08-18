module Hasql.Comms.Session.CleanUpAfterFailureSpec (spec) where

import Hasql.Comms.Session qualified as Session
import Hasql.Platform.Prelude
import Pqi qualified as Pq
import Pqi.Ffi qualified
import Test.Hspec
import TextBuilder qualified

-- | 'Session.cleanUpAfterFailure' is the light repair 'Hasql.Connection.use'
-- runs on every ordinary 'Left' return, standing in for the exit-failure
-- hole that used to live in "Hasql.Comms.Roundtrip.toPipelineIO": when
-- 'Pq.exitPipelineMode' failed there, the code reported an error and handed
-- the connection back still in pipeline mode, with nothing downstream ever
-- checking for that again. This exercises that 'cleanUpAfterFailure' alone -
-- without the cancel, ABORT and DEALLOCATE ALL steps 'cleanUpAfterInterruption'
-- also performs - is enough to bring a connection left in that state back to
-- a usable one.
--
-- It is also what makes pipeline mode scoped: whoever turns it on owes the
-- connection a way out of it, on the failing paths as much as on the
-- succeeding one. The hard case is a mode left on with commands already
-- dispatched and unanswered - what a send failing halfway through a batched
-- pipeline leaves behind
-- (<https://github.com/nikita-volkov/hasql/issues/326>). The server
-- withholds those results until it sees a Sync, and @PQexitPipelineMode@
-- refuses while any of them are still queued, so getting out takes a Sync, a
-- Flush and a drain that runs command boundary by command boundary rather
-- than in one pass.
spec :: SpecWith (Text, Word16)
spec = do
  describe "cleanUpAfterFailure" do
    it "is a no-op when the connection is already clean" \config -> do
      withConnection config \connection -> do
        status <- Pq.pipelineStatus connection
        status `shouldBe` Pq.PipelineOff

        result <- Session.toHandler Session.cleanUpAfterFailure connection
        result `shouldBe` Right ()

        status <- Pq.pipelineStatus connection
        status `shouldBe` Pq.PipelineOff

    it "leaves pipeline mode when the connection was left in it with nothing dispatched" \config -> do
      withConnection config \connection -> do
        success <- Pq.enterPipelineMode connection
        success `shouldBe` True

        status <- Pq.pipelineStatus connection
        status `shouldBe` Pq.PipelineOn

        result <- Session.toHandler Session.cleanUpAfterFailure connection
        result `shouldBe` Right ()

        status <- Pq.pipelineStatus connection
        status `shouldBe` Pq.PipelineOff

    it "leaves a pipeline mode holding unanswered commands" \config -> do
      withConnection config \connection -> do
        entered <- Pq.enterPipelineMode connection
        entered `shouldBe` True

        -- Dispatched without a Sync, exactly as a batched pipeline whose
        -- send failed before reaching its own Sync leaves them.
        dispatch connection 3

        result <- Session.toHandler Session.cleanUpAfterFailure connection
        result `shouldBe` Right ()

        status <- Pq.pipelineStatus connection
        status `shouldBe` Pq.PipelineOff

    it "leaves the connection able to serve serial commands again" \config -> do
      -- The point of leaving the mode at all: while it is on, libpq refuses
      -- every serial command, and the results of the dispatched ones would
      -- be read by whoever queries next as if they were their own.
      withConnection config \connection -> do
        entered <- Pq.enterPipelineMode connection
        entered `shouldBe` True
        dispatch connection 3

        result <- Session.toHandler Session.cleanUpAfterFailure connection
        result `shouldBe` Right ()

        execResultStatus connection "select 'after' as marker"
          `shouldReturn` Just Pq.TuplesOk

    it "is idempotent" \config -> do
      withConnection config \connection -> do
        entered <- Pq.enterPipelineMode connection
        entered `shouldBe` True
        dispatch connection 3

        firstResult <- Session.toHandler Session.cleanUpAfterFailure connection
        firstResult `shouldBe` Right ()

        secondResult <- Session.toHandler Session.cleanUpAfterFailure connection
        secondResult `shouldBe` Right ()

        execResultStatus connection "select 1" `shouldReturn` Just Pq.TuplesOk

    it "leaves a pipeline mode that a Sync already terminated" \config -> do
      -- The results are sitting in the client's buffer rather than
      -- withheld by the server, so the drain has to consume them without
      -- the Sync of its own contributing anything to consume.
      withConnection config \connection -> do
        entered <- Pq.enterPipelineMode connection
        entered `shouldBe` True
        dispatch connection 3
        synced <- Pq.pipelineSync connection
        synced `shouldBe` True

        result <- Session.toHandler Session.cleanUpAfterFailure connection
        result `shouldBe` Right ()

        status <- Pq.pipelineStatus connection
        status `shouldBe` Pq.PipelineOff

    it "leaves an aborted pipeline" \config -> do
      -- One failing command puts the pipeline into the aborted status, in
      -- which the server discards every command up to the next Sync. That
      -- is still pipeline mode, and it still has to come off.
      withConnection config \connection -> do
        entered <- Pq.enterPipelineMode connection
        entered `shouldBe` True

        sent <- Pq.sendQueryParams connection "select 1 from nonexistent_relation" [] Pq.Binary
        sent `shouldBe` True
        dispatch connection 2
        synced <- Pq.pipelineSync connection
        synced `shouldBe` True

        -- Consume up to the failing command's boundary so that the mode is
        -- observably aborted before the repair runs.
        void (Pq.getResult connection)
        void (Pq.getResult connection)
        Pq.pipelineStatus connection `shouldReturn` Pq.PipelineAborted

        result <- Session.toHandler Session.cleanUpAfterFailure connection
        result `shouldBe` Right ()

        status <- Pq.pipelineStatus connection
        status `shouldBe` Pq.PipelineOff

        execResultStatus connection "select 1" `shouldReturn` Just Pq.TuplesOk

    it "leaves an open transaction untouched, unlike cleanUpAfterInterruption" \config -> do
      withConnection config \connection -> do
        _ <- Pq.exec connection "BEGIN"

        transStatus <- Pq.transactionStatus connection
        transStatus `shouldBe` Pq.TransInTrans

        result <- Session.toHandler Session.cleanUpAfterFailure connection
        result `shouldBe` Right ()

        transStatus <- Pq.transactionStatus connection
        transStatus `shouldBe` Pq.TransInTrans

        -- Leave the connection clean for the bracket's teardown.
        _ <- Pq.exec connection "ROLLBACK"
        pure ()

-- * Helpers

-- | Dispatch the given amount of trivial commands into an open pipeline
-- without syncing it.
dispatch :: Pq.Connection -> Int -> IO ()
dispatch connection amount =
  replicateM_ amount do
    sent <- Pq.sendQueryParams connection "select 1" [] Pq.Binary
    sent `shouldBe` True

-- | Run a serial command and report the status of its result, if any.
execResultStatus :: Pq.Connection -> ByteString -> IO (Maybe Pq.ExecStatus)
execResultStatus connection sql =
  Pq.exec connection sql >>= traverse Pq.resultStatus

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
        (Pq.connectdb Pqi.Ffi.adapter connectionString)
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
