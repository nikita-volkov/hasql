module Hasql.Comms.Helpers.ConnOpsSpec (spec) where

import Hasql.Comms.Helpers.ConnOps qualified as ConnOps
import Hasql.Platform.Prelude
import Pqi qualified as Pq
import Pqi.Ffi qualified
import Test.Hspec
import TextBuilder qualified

-- | 'ConnOps.leave' is what makes pipeline mode scoped: whoever turned
-- it on owes the connection a way out of it, on the failing paths as much as
-- on the succeeding one.
--
-- The hard case is a mode left on with commands already dispatched and
-- unanswered - what a send failing halfway through a batched pipeline leaves
-- behind (<https://github.com/nikita-volkov/hasql/issues/326>). The server
-- withholds those results until it sees a Sync, and @PQexitPipelineMode@
-- refuses while any of them are still queued, so getting out takes a Sync, a
-- Flush and a drain that runs command boundary by command boundary rather
-- than in one pass.
spec :: SpecWith (Text, Word16)
spec = do
  describe "leave" do
    it "Is a no-op on a connection that is not in pipeline mode" \config -> do
      withConnection config \connection -> do
        result <- ConnOps.leave connection
        result `shouldBe` Right ()

        status <- Pq.pipelineStatus connection
        status `shouldBe` Pq.PipelineOff

    it "Leaves a pipeline mode with nothing dispatched in it" \config -> do
      withConnection config \connection -> do
        entered <- Pq.enterPipelineMode connection
        entered `shouldBe` True

        result <- ConnOps.leave connection
        result `shouldBe` Right ()

        status <- Pq.pipelineStatus connection
        status `shouldBe` Pq.PipelineOff

    it "Leaves a pipeline mode holding unanswered commands" \config -> do
      withConnection config \connection -> do
        entered <- Pq.enterPipelineMode connection
        entered `shouldBe` True

        -- Dispatched without a Sync, exactly as a batched pipeline whose
        -- send failed before reaching its own Sync leaves them.
        dispatch connection 3

        result <- ConnOps.leave connection
        result `shouldBe` Right ()

        status <- Pq.pipelineStatus connection
        status `shouldBe` Pq.PipelineOff

    it "Leaves the connection able to serve serial commands again" \config -> do
      -- The point of leaving the mode at all: while it is on, libpq refuses
      -- every serial command, and the results of the dispatched ones would
      -- be read by whoever queries next as if they were their own.
      withConnection config \connection -> do
        entered <- Pq.enterPipelineMode connection
        entered `shouldBe` True
        dispatch connection 3

        result <- ConnOps.leave connection
        result `shouldBe` Right ()

        execResultStatus connection "select 'after' as marker"
          `shouldReturn` Just Pq.TuplesOk

    it "Is idempotent" \config -> do
      -- 'Hasql.Comms.Roundtrip.toPipelineIO' calls it on the ordinary path
      -- too, after its own exit already succeeded, and
      -- 'Hasql.Comms.Session.cleanUpAfterFailure' may call it again after
      -- that. Neither must find anything left to do.
      withConnection config \connection -> do
        entered <- Pq.enterPipelineMode connection
        entered `shouldBe` True
        dispatch connection 3

        firstResult <- ConnOps.leave connection
        firstResult `shouldBe` Right ()

        secondResult <- ConnOps.leave connection
        secondResult `shouldBe` Right ()

        execResultStatus connection "select 1" `shouldReturn` Just Pq.TuplesOk

    it "Leaves a pipeline mode that a Sync already terminated" \config -> do
      -- The results are sitting in the client's buffer rather than
      -- withheld by the server, so the drain has to consume them without
      -- the Sync of its own contributing anything to consume.
      withConnection config \connection -> do
        entered <- Pq.enterPipelineMode connection
        entered `shouldBe` True
        dispatch connection 3
        synced <- Pq.pipelineSync connection
        synced `shouldBe` True

        result <- ConnOps.leave connection
        result `shouldBe` Right ()

        status <- Pq.pipelineStatus connection
        status `shouldBe` Pq.PipelineOff

    it "Leaves an aborted pipeline" \config -> do
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

        result <- ConnOps.leave connection
        result `shouldBe` Right ()

        status <- Pq.pipelineStatus connection
        status `shouldBe` Pq.PipelineOff

        execResultStatus connection "select 1" `shouldReturn` Just Pq.TuplesOk

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
