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
spec :: SpecWith (Text, Word16)
spec = do
  describe "cleanUpAfterFailure" do
    it "leaves pipeline mode when the connection was left in it" \config -> do
      withConnection config \connection -> do
        success <- Pq.enterPipelineMode connection
        success `shouldBe` True

        status <- Pq.pipelineStatus connection
        status `shouldBe` Pq.PipelineOn

        result <- Session.toHandler Session.cleanUpAfterFailure connection
        result `shouldBe` Right ()

        status <- Pq.pipelineStatus connection
        status `shouldBe` Pq.PipelineOff

    it "is a no-op when the connection is already clean" \config -> do
      withConnection config \connection -> do
        status <- Pq.pipelineStatus connection
        status `shouldBe` Pq.PipelineOff

        result <- Session.toHandler Session.cleanUpAfterFailure connection
        result `shouldBe` Right ()

        status <- Pq.pipelineStatus connection
        status `shouldBe` Pq.PipelineOff

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
