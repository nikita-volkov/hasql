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

-- | 'Session.Session'\'s 'Control.Monad.Except.MonadError' instance ranges
-- over 'Errors.SessionError' only, so a pipeline send failure -
-- 'Errors.ConnectionUseError' or 'Errors.DriverUseError' - is not a value
-- 'Control.Monad.Except.catchError' can see. The handler here never runs:
-- '>>=' short-circuits on the fatal error before the handler is reached,
-- exactly as it would for any other 'Left', so the rest of the session
-- never runs against a connection still in pipeline mode, and
-- 'Hasql.Connection.use' gets the fatal error back whole.
--
-- Before the split this was the test of the opposite risk: a session that
-- caught the failure and swallowed it, leaving a serial command blocking
-- forever on results the server was never asked for. That risk is gone by
-- construction now, which is what this spec pins.
spec :: SpecWith Scripts.ScopeParams
spec = do
  describe "Pipeline send failure 'caught' within the session" do
    it "Never reaches the handler, and the rest of the session never runs" \config -> do
      Scripts.onUnpreparableConnection config \connection -> do
        result <-
          Connection.use connection do
            catchError (Session.pipeline failingPipeline) (const (pure ()))
            Execution.sessionByParams Statements.SelectOne

        case result of
          Left (Errors.DriverUseError _) -> pure ()
          _ -> expectationFailure ("Unexpected result: " <> show result)

    it "Finishes the connection, not merely reports the error" \config -> do
      Scripts.onUnpreparableConnection config \connection -> do
        result <-
          Connection.use connection do
            catchError (Session.pipeline failingPipeline) (const (pure ()))

        case result of
          Left (Errors.DriverUseError _) -> pure ()
          _ -> expectationFailure ("Unexpected result: " <> show result)

        followUp <-
          Connection.use connection
            $ Execution.sessionByParams Statements.SelectOne
        case followUp of
          Left (Errors.ConnectionUseError _) -> pure ()
          _ -> expectationFailure ("Unexpected follow-up result: " <> show followUp)
