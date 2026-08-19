module Integration.Sharing.Session.RecoveredPipelineFailureSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Errors qualified as Errors
import Hasql.Session qualified as Session
import Helpers.Dsls.Execution qualified as Execution
import Helpers.Scripts qualified as Scripts
import Helpers.Statements qualified as Statements
import Prelude
import Test.Hspec

-- | A pipeline of five statements with 'Statements.TooManyParams' in the
-- middle, so that the batched send of the whole pipeline fails halfway
-- through and leaves the connection in pipeline mode with commands queued
-- behind the refusal.
failingPipeline :: Execution.Pipeline ()
failingPipeline = do
  _ <- goodStatement
  _ <- goodStatement
  _ <- Execution.pipelineByParams Statements.TooManyParams
  _ <- goodStatement
  _ <- goodStatement
  pure ()
  where
    goodStatement =
      Execution.pipelineByParams Statements.GenerateSeries {start = 0, end = 2}

-- | A 'Session.Session' is a 'Control.Monad.Except.MonadError', so it can
-- catch a pipeline failure and carry on - which is why "the connection is
-- finished when the session returns" is not on its own enough. By the time
-- 'Hasql.Connection.use' sees anything, the rest of the session has already
-- run against the connection the send failed on, and a serial command
-- issued while it is still in pipeline mode does not fail: it blocks
-- forever waiting on results the server was never asked for, and the block
-- does not respond to 'System.Timeout.timeout' either, so a suite that gets
-- there hangs instead of reporting.
--
-- So the verdict is recorded in the connection state at the point of
-- failure. The remainder of the session is refused rather than run, and the
-- connection is finished afterwards however the session chose to end.
spec :: SpecWith Scripts.ScopeParams
spec = do
  describe "Pipeline send failure caught within the session" do
    it "Refuses the rest of the session rather than running it on the connection" \config -> do
      Scripts.onUnpreparableConnection config \connection -> do
        result <-
          Connection.use connection do
            catchError (Session.pipeline failingPipeline) (const (pure ()))
            Session.script "select 1"

        case result of
          Left (Errors.ConnectionSessionError _) -> pure ()
          _ -> expectationFailure ("Unexpected result: " <> show result)

    it "Refuses a statement in the same session too" \config -> do
      Scripts.onUnpreparableConnection config \connection -> do
        result <-
          Connection.use connection do
            catchError (Session.pipeline failingPipeline) (const (pure ()))
            Execution.sessionByParams Statements.SelectOne

        case result of
          Left (Errors.ConnectionSessionError _) -> pure ()
          _ -> expectationFailure ("Unexpected result: " <> show result)

    it "Finishes the connection even when the session goes on to succeed" \config -> do
      -- The sharpest form of it: the session swallows the failure and
      -- returns 'Right', so the error never reaches 'Hasql.Connection.use'
      -- at all. The connection still must not be handed back for reuse.
      Scripts.onUnpreparableConnection config \connection -> do
        result <-
          Connection.use connection do
            catchError (Session.pipeline failingPipeline) (const (pure ()))

        result `shouldBe` Right ()

        followUp <-
          Connection.use connection
            $ Execution.sessionByParams Statements.SelectOne
        case followUp of
          Left (Errors.ConnectionSessionError _) -> pure ()
          _ -> expectationFailure ("Unexpected follow-up result: " <> show followUp)
