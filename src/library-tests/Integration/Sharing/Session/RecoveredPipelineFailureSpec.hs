module Integration.Sharing.Session.RecoveredPipelineFailureSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Dsls.Execution qualified as Execution
import Helpers.Scripts qualified as Scripts
import Helpers.Statements qualified as Statements
import Pqi qualified as Pq
import Prelude
import Test.Hspec

-- | A statement with more parameters than libpq accepts in a single command.
--
-- See "Integration.Sharing.Pipeline.Statement.SendFailureSpec" for the full
-- explanation: placed in the middle of a pipeline it makes the batched send
-- of the whole pipeline fail halfway through, leaving the connection in
-- pipeline mode with the preceding commands dispatched and unanswered.
tooManyParamsStatement :: Statement.Statement () ()
tooManyParamsStatement =
  Statement.unpreparable
    "select 1"
    (foldMap (const int8Param) [1 .. paramCount])
    Decoders.noResult
  where
    paramCount = 65536 :: Int
    int8Param = contramap (const 1) (Encoders.param (Encoders.nonNullable Encoders.int8))

-- | A pipeline of five statements with 'tooManyParamsStatement' in the middle.
failingPipeline :: Pipeline.Pipeline ()
failingPipeline = do
  _ <- goodStatement
  _ <- goodStatement
  _ <- Pipeline.statement () tooManyParamsStatement
  _ <- goodStatement
  _ <- goodStatement
  pure ()
  where
    goodStatement =
      Execution.pipelineByParams Statements.GenerateSeries {start = 0, end = 2}

-- | Read the pipeline status of the raw connection without sending anything
-- on it.
--
-- The specs below assert on this rather than on the outcome of a follow-up
-- serial statement, because issuing one while the connection is still in
-- pipeline mode does not fail - it blocks forever waiting on results the
-- server will never send, and the block is not interruptible by
-- 'System.Timeout.timeout'. A test written that way hangs the whole suite
-- instead of reporting.
pipelineStatusSession :: Session.Session Pq.PipelineStatus
pipelineStatusSession =
  Session.onLibpqConnection \pqConnection -> do
    status <- Pq.pipelineStatus pqConnection
    pure (Right status, pqConnection)

-- | 'Hasql.Connection.use' repairs a connection left in pipeline mode only
-- after the session has returned, and only when it returns 'Left'.
--
-- But 'Session.Session' is a 'MonadError' over the connection state, so a
-- session can catch a pipeline failure and carry on - or catch it and
-- succeed. In the first case the rest of the session runs against a
-- connection still in pipeline mode; in the second no repair runs at all and
-- that connection is handed back for reuse. Either way the state
-- 'Hasql.Connection.use' is supposed to own escapes it.
spec :: SpecWith Scripts.ScopeParams
spec = do
  describe "Pipeline send failure recovered from within the session" do
    it "Leaves pipeline mode on for the rest of the session" \config -> do
      Scripts.onUnpreparableConnection config \connection -> do
        result <-
          Connection.use connection do
            catchError (Session.pipeline failingPipeline) (const (pure ()))
            pipelineStatusSession

        result `shouldBe` Right Pq.PipelineOff

    it "Hands the connection back in pipeline mode when the session goes on to succeed" \config -> do
      Scripts.onUnpreparableConnection config \connection -> do
        result <-
          Connection.use connection do
            catchError (Session.pipeline failingPipeline) (const (pure ()))

        result `shouldBe` Right ()

        followUp <- Connection.use connection pipelineStatusSession

        followUp `shouldBe` Right Pq.PipelineOff

    it "Corrupts the results of a later statement in the same session" \config -> do
      Scripts.onUnpreparableConnection config \connection -> do
        -- A follow-up pipeline is not refused outright: it re-enters the
        -- (already on) pipeline mode and reads the stale results the failed
        -- pipeline left queued, decoding another statement's rows as its
        -- own. 'SelectOne' selects an int4, but what comes back is a row of
        -- the earlier 'GenerateSeries', whose column is an int8.
        result <-
          Connection.use connection do
            catchError (Session.pipeline failingPipeline) (const (pure ()))
            Execution.sessionByParams Statements.SelectOne

        result `shouldBe` Right 1
