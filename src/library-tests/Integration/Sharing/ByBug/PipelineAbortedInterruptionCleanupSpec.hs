module Integration.Sharing.ByBug.PipelineAbortedInterruptionCleanupSpec (spec) where

import Data.Text qualified as Text
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Prelude
import Test.Hspec

-- | A statement that sleeps for the given number of seconds and succeeds.
--
-- Used to widen the wall-clock window during which the client is blocked
-- waiting on the network/socket for a pipelined result, so that an
-- asynchronous interruption has a realistic chance of landing right around
-- the moment the *next* statement in the same pipeline fails.
sleepStatement :: Statement.Statement Double ()
sleepStatement =
  Statement.preparable
    "select pg_sleep($1)"
    (Encoders.param (Encoders.nonNullable Encoders.float8))
    Decoders.noResult

-- | A statement that is guaranteed to fail on the server.
failingStatement :: Statement.Statement () ()
failingStatement =
  Statement.preparable
    "select 1/0"
    Encoders.noParams
    Decoders.noResult

-- | A pipeline of two statements: the first sleeps and succeeds, the
-- second fails. Executing this via 'Session.pipeline' drives libpq's
-- pipeline status through: Off -> On -> (once the sleep result has been
-- received and the divide error has been processed) Aborted -> (normally)
-- Off again, via the exit sequence inside 'toPipelineIO'.
--
-- The bug under test concerns what happens if an asynchronous exception
-- interrupts execution during the narrow "Aborted" window: right after
-- libpq has registered the error result for the second statement (which
-- flips its internal pipeline status to `PipelineAborted`) but before the
-- driver has drained the trailing pipeline-sync marker and called
-- `exitPipelineMode`. That window is only a couple of FFI calls wide, so
-- reliably landing an async exception inside it requires many attempts
-- across a fine-grained sweep of interrupt delays (see 'spec' below).
--
-- Note: an earlier version of this test tried to widen the window by
-- appending many trivial "filler" statements after the failing one (on
-- the theory that draining their results would take measurably longer).
-- That approach reproduced failures reliably, but for the wrong reason:
-- `Comms.Session.drainResults` only drains one queued command's worth of
-- results per call, so a large backlog of undrained filler results made
-- `exitPipelineMode` fail with "cannot exit pipeline mode with uncollected
-- results" regardless of whether the `PipelineOn`/`PipelineAborted` bug
-- under test was present or fixed. That's a real, separate limitation of
-- `drainResults`, but not the bug this test is about, so the pipeline here
-- is kept to exactly two statements and 'attempt' below specifically
-- checks for the "not allowed in pipeline mode" signature (the one the
-- one-line `leavePipeline` fix actually addresses) rather than any
-- "Failed to clean up after interruption" message.
racingPipelineSession :: Double -> Session.Session ()
racingPipelineSession sleepSeconds =
  Session.pipeline do
    Pipeline.statement sleepSeconds sleepStatement
      *> Pipeline.statement () failingStatement

-- | Try once to reproduce the bug: acquire a fresh connection, race a
-- `timeout` against the pipelined session (sleep-then-fail) tuned to fire
-- right around the moment the pipeline transitions to the aborted state,
-- and report whether `Connection.use` came back with the specific driver
-- error that signals the `leavePipeline` bug: it only checks for
-- `PipelineOn`, so when the connection is genuinely `PipelineAborted` at
-- interruption time, cleanup skips leaving the pipeline and falls through
-- to `bringTransactionStatusToIdle`, which tries to send "ABORT" as a
-- serial command while still in pipeline mode -- something libpq flatly
-- refuses ("PQsendQuery not allowed in pipeline mode").
--
-- Note on why checking `Connection.use`'s own return value is enough: when
-- `timeout` throws its internal exception into the thread running
-- `Connection.use`, that exception is caught by `use`'s own
-- @try \@SomeException@. If the bug is NOT triggered, `use` cleans up
-- successfully and rethrows the very same timeout exception, so `timeout`
-- observes it and returns 'Nothing'. If the bug IS triggered, `use`
-- reports the cleanup failure as an ordinary `Left (DriverSessionError _)`
-- return value instead of rethrowing, so `timeout` observes a normal
-- return and reports 'Just (Left _)'.
attempt :: Scripts.ScopeParams -> Double -> Int -> IO (Maybe Text)
attempt config sleepSeconds delayMicros =
  Scripts.onPreparableConnection config \connection -> do
    result <- timeout delayMicros do
      Connection.use connection (racingPipelineSession sleepSeconds)
    pure case result of
      Just (Left err) ->
        let rendered = Text.pack (show err)
         in if "Failed to clean up after interruption"
              `Text.isInfixOf` rendered
              && "not allowed in pipeline mode"
              `Text.isInfixOf` rendered
              then Just rendered
              else Nothing
      Just (Right ()) -> Nothing
      Nothing -> Nothing

spec :: SpecWith Scripts.ScopeParams
spec = do
  describe "Interruption of a pipeline while it is in the Aborted status" do
    it "Connection.use recovers cleanly instead of reporting a driver cleanup failure" \config -> do
      -- We sweep the timeout across a window that straddles the moment the
      -- sleep statement finishes and the failing statement's error result
      -- gets processed by libpq (which is when the pipeline status flips
      -- from `PipelineOn` to `PipelineAborted`). The genuinely vulnerable
      -- window is only a couple of FFI calls wide (nowhere near as wide as
      -- our timer granularity), so we compensate with a large number of
      -- attempts spread finely across the window and a fresh connection
      -- each time, rather than trying to widen the window itself.
      let sleepMicros = 20000 :: Int -- 20ms sleep statement duration
          sleepSeconds = fromIntegral sleepMicros / 1000000
          delays = [sleepMicros + step | step <- [(-3000), (-2900) .. 6000]]
          attemptsPerDelay = 15

      results <-
        sequence
          [ attempt config sleepSeconds d
          | d <- delays,
            _ <- [1 :: Int .. attemptsPerDelay]
          ]

      let reproductions = [msg | Just msg <- results]

      reproductions
        `shouldBe` []
