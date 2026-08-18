module Integration.Sharing.Session.RecoveredPipelineFailureSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Session qualified as Session
import Helpers.Dsls.Execution qualified as Execution
import Helpers.Scripts qualified as Scripts
import Helpers.Statements qualified as Statements
import Pqi qualified as Pq
import Prelude
import Test.Hspec

-- | A pipeline of five statements with 'Statements.TooManyParams' in the
-- middle, so that the batched send of the whole pipeline fails halfway
-- through and leaves the connection in pipeline mode with the preceding
-- commands dispatched and unanswered.
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

-- | Pipeline mode is scoped to one pipeline, and
-- 'Hasql.Comms.Roundtrip.toPipelineIO' leaves it before returning on every
-- path - including a send that failed halfway through the batch.
--
-- It has to happen there rather than in 'Hasql.Connection.use', which used
-- to own it: 'Session.Session' is a 'MonadError' over the connection state,
-- so a session can catch a pipeline failure and carry on, in which case the
-- rest of it ran against a connection still in pipeline mode, or catch it
-- and succeed, in which case the repair in 'Hasql.Connection.use' never ran
-- and the connection was handed back for reuse still dirty.
spec :: SpecWith Scripts.ScopeParams
spec = do
  describe "Pipeline send failure recovered from within the session" do
    it "Leaves pipeline mode off for the rest of the session" \config -> do
      Scripts.onUnpreparableConnection config \connection -> do
        result <-
          Connection.use connection do
            catchError (Session.pipeline failingPipeline) (const (pure ()))
            pipelineStatusSession

        result `shouldBe` Right Pq.PipelineOff

    it "Hands the connection back with pipeline mode off when the session goes on to succeed" \config -> do
      Scripts.onUnpreparableConnection config \connection -> do
        result <-
          Connection.use connection do
            catchError (Session.pipeline failingPipeline) (const (pure ()))

        result `shouldBe` Right ()

        followUp <- Connection.use connection pipelineStatusSession

        followUp `shouldBe` Right Pq.PipelineOff

    it "Lets a serial command run in the same session afterwards" \config -> do
      Scripts.onUnpreparableConnection config \connection -> do
        -- The sharpest form of the bug, and the reason the specs around it
        -- assert on 'Pq.pipelineStatus' rather than on this: while the mode
        -- was still on, a serial command did not fail, it blocked forever
        -- waiting on results the server never sent - and the block did not
        -- respond to 'System.Timeout.timeout' either, so a suite written
        -- this way hung instead of reporting.
        result <-
          Connection.use connection do
            catchError (Session.pipeline failingPipeline) (const (pure ()))
            Session.script "select 1"

        result `shouldBe` Right ()

    it "Does not corrupt the results of a later statement in the same session" \config -> do
      Scripts.onUnpreparableConnection config \connection -> do
        -- A follow-up pipeline used not to be refused outright: it
        -- re-entered the (already on) pipeline mode and read the stale
        -- results the failed pipeline had left queued, decoding another
        -- statement's rows as its own. 'SelectOne' selects an int4, and what
        -- came back was a row of the earlier 'GenerateSeries', whose column
        -- is an int8 - a mismatch the decoder happened to catch, but two
        -- statements of matching column types would have silently returned
        -- the wrong value.
        result <-
          Connection.use connection do
            catchError (Session.pipeline failingPipeline) (const (pure ()))
            Execution.sessionByParams Statements.SelectOne

        result `shouldBe` Right 1
