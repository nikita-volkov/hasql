module Integration.Sharing.Pipeline.Statement.SendFailureSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Errors qualified as Errors
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Helpers.Dsls.Execution qualified as Execution
import Helpers.Scripts qualified as Scripts
import Helpers.Statements qualified as Statements
import Pqi qualified as Pq
import Prelude
import Test.Hspec

-- | A pipeline of five statements with 'Statements.TooManyParams' in the
-- middle: statements 1 and 2 get dispatched to the server, statement 3 fails
-- on send, statements 4 and 5 never reach it.
--
-- This is the reproduction of <https://github.com/nikita-volkov/hasql/issues/326>.
failingPipeline :: Pipeline.Pipeline ()
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

-- | The same failure with nothing dispatched before it: the send of the very
-- first statement fails, so the pipeline mode is on and the command queue is
-- empty.
failingOnFirstStatementPipeline :: Pipeline.Pipeline ()
failingOnFirstStatementPipeline = do
  _ <- Execution.pipelineByParams Statements.TooManyParams
  _ <- Execution.pipelineByParams Statements.GenerateSeries {start = 0, end = 2}
  pure ()

spec :: SpecWith Scripts.ScopeParams
spec = do
  describe "Send failure in the middle of a pipeline" do
    it "Captures the error" \config -> do
      Scripts.onUnpreparableConnection config runFailingPipeline

    it "Reports it as non-transient" \config -> do
      Scripts.onUnpreparableConnection config \connection -> do
        -- libpq refused the request itself without touching the socket, so
        -- the connection is fine and the same request will be refused the
        -- same way on any other one. Reporting this as transient is what
        -- made retry wrappers spin on it forever.
        result <- Connection.use connection (Session.pipeline failingPipeline)
        case result of
          Left err -> Errors.isTransient err `shouldBe` False
          Right () -> expectationFailure "The pipeline unexpectedly succeeded"

    it "Leaves the connection usable" \config -> do
      Scripts.onUnpreparableConnection config \connection -> do
        -- A mid-pipeline send failure must not leave the connection in
        -- pipeline mode. Issue #326 reports that when it does, libpq
        -- rejects every subsequent command sent on it, so every later
        -- session on the same connection fails too - even though the
        -- connection itself is otherwise perfectly usable.
        runFailingPipeline connection
        followUpResult <-
          Connection.use connection
            $ Session.script "select 1"
        shouldBe followUpResult (Right ())

    it "Leaves pipeline mode off" \config -> do
      Scripts.onUnpreparableConnection config \connection -> do
        runFailingPipeline connection
        status <- Connection.use connection pipelineStatusSession
        shouldBe status (Right Pq.PipelineOff)

    it "Recovers from it as many times as it happens" \config -> do
      -- The repair is a step of every pipeline round trip, not a one-off
      -- rescue, so a connection that has already been through it must go
      -- through it again just as well.
      Scripts.onUnpreparableConnection config \connection -> do
        runFailingPipeline connection
        runFailingPipeline connection
        runFailingPipeline connection
        followUpResult <-
          Connection.use connection
            $ Execution.sessionByParams Statements.GenerateSeries {start = 0, end = 2}
        shouldBe followUpResult (Right [0, 1, 2])

    describe "When it happens on the first statement of the pipeline" do
      it "Leaves the connection usable" \config -> do
        -- Nothing was dispatched before the failure, so the repair has an
        -- empty command queue to drain - the opposite end of the range from
        -- the mid-pipeline case above.
        Scripts.onUnpreparableConnection config \connection -> do
          result <-
            Connection.use connection
              $ Session.pipeline failingOnFirstStatementPipeline
          case result of
            Left (Errors.DriverSessionError _) -> pure ()
            _ -> expectationFailure ("Unexpected pipeline result: " <> show result)
          followUpResult <-
            Connection.use connection
              $ Session.script "select 1"
          shouldBe followUpResult (Right ())

    describe "On a connection with prepared statements enabled" do
      it "Leaves the statement cache agreeing with the server" \config -> do
        -- The statements preceding the failure were dispatched as PARSE plus
        -- EXECUTE and the repair syncs them, so the server does hold them
        -- prepared. If the driver were to drop them from its cache it would
        -- re-issue PARSE under the same content-addressed name and get 42P05;
        -- if it were to keep names the server does not hold it would get
        -- 26000. Re-running the same statements catches either.
        Scripts.onPreparableConnection config \connection -> do
          runFailingPipeline connection
          followUpResult <-
            Connection.use connection
              $ Execution.sessionByParams Statements.GenerateSeries {start = 0, end = 2}
          shouldBe followUpResult (Right [0, 1, 2])
          rerunResult <-
            Connection.use connection
              $ Execution.sessionByParams Statements.GenerateSeries {start = 0, end = 2}
          shouldBe rerunResult (Right [0, 1, 2])

    describe "Effects on the database state" do
      it "Keeps the effects of the statements preceding the failure and skips the following ones" \config -> do
        -- Unlike a query error, which reaches the server before the Sync
        -- ending the pipeline's implicit transaction block and so rolls the
        -- whole block back, a send failure is discovered on the client with
        -- the preceding commands already on the wire. Recovering the
        -- connection means sending that Sync, which commits them. The
        -- statements after the failure were never sent at all.
        Scripts.onPreparableConnection config \connection -> do
          varname <- Execution.generateVarname
          let setVar value =
                Execution.pipelineByParams
                  Statements.SetConfig {name = varname, value, local = False}
          result <-
            (Connection.use connection . Session.pipeline)
              $ (,,)
              <$> setVar "before"
              <*> Execution.pipelineByParams Statements.TooManyParams
              <*> setVar "after"
          case result of
            Left (Errors.DriverSessionError _) -> pure ()
            _ -> expectationFailure ("Unexpected result: " <> show result)
          settingResult <-
            Connection.use connection
              $ Execution.sessionByParams Statements.CurrentSetting {name = varname, missingOk = True}
          shouldBe settingResult (Right (Just "before"))

    describe "Inside an explicit transaction" do
      it "Leaves the transaction open rather than aborting it" \config -> do
        -- Recovery here is the light repair, not the one run after an
        -- interruption: it takes the connection out of pipeline mode and
        -- stops there. A transaction the caller opened is the caller's to
        -- end, and a statement failing inside one is an ordinary thing to
        -- catch and carry on from.
        Scripts.onPreparableConnection config \connection -> do
          beginResult <- Connection.use connection (Session.script "begin")
          shouldBe beginResult (Right ())

          runFailingPipeline connection

          status <- Connection.use connection transactionStatusSession
          shouldBe status (Right Pq.TransInTrans)

          rollbackResult <- Connection.use connection (Session.script "rollback")
          shouldBe rollbackResult (Right ())

-- | Run 'failingPipeline' and assert it fails with the expected rejection.
runFailingPipeline :: Connection.Connection -> IO ()
runFailingPipeline connection = do
  result <-
    Connection.use connection
      $ Session.pipeline failingPipeline
  case result of
    Left (Errors.DriverSessionError _) -> pure ()
    _ -> expectationFailure ("Unexpected pipeline result: " <> show result)

-- | Read the pipeline status of the raw connection without sending anything
-- on it.
pipelineStatusSession :: Session.Session Pq.PipelineStatus
pipelineStatusSession =
  Session.onLibpqConnection \pqConnection -> do
    status <- Pq.pipelineStatus pqConnection
    pure (Right status, pqConnection)

-- | Read the transaction status of the raw connection without sending
-- anything on it.
transactionStatusSession :: Session.Session Pq.TransactionStatus
transactionStatusSession =
  Session.onLibpqConnection \pqConnection -> do
    status <- Pq.transactionStatus pqConnection
    pure (Right status, pqConnection)
