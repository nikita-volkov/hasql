module Integration.Sharing.Pipeline.Statement.SendFailureSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Errors qualified as Errors
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Dsls.Execution qualified as Execution
import Helpers.Scripts qualified as Scripts
import Helpers.Statements qualified as Statements
import Prelude
import Test.Hspec

-- | A statement with more parameters than libpq accepts in a single command.
--
-- @PQsendQueryParams@ rejects a parameter list longer than 65535 locally,
-- without writing anything to the socket. Placed in the middle of a pipeline,
-- this makes the batched send of the whole pipeline fail halfway through:
-- the commands preceding the failing statement have already been dispatched,
-- while the failing one and everything after it never reach the server.
--
-- This is the reproduction trigger of
-- <https://github.com/nikita-volkov/hasql/issues/326>.
tooManyParamsStatement :: Statement.Statement () ()
tooManyParamsStatement =
  Statement.unpreparable
    "select 1"
    (foldMap (const int8Param) [1 .. paramCount])
    Decoders.noResult
  where
    -- One more than the maximum amount of parameters libpq accepts.
    paramCount = 65536 :: Int

    int8Param = contramap (const 1) (Encoders.param (Encoders.nonNullable Encoders.int8))

-- | A pipeline of five statements with 'tooManyParamsStatement' in the middle:
-- statements 1 and 2 get dispatched to the server, statement 3 fails on send,
-- statements 4 and 5 never reach it.
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

spec :: SpecWith Scripts.ScopeParams
spec = do
  describe "Send failure in the middle of a pipeline" do
    it "Captures the error" \config -> do
      Scripts.onUnpreparableConnection config runFailingPipeline

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

-- | Run 'failingPipeline' and assert it fails with the expected rejection.
runFailingPipeline :: Connection.Connection -> IO ()
runFailingPipeline connection = do
  result <-
    Connection.use connection
      $ Session.pipeline failingPipeline
  case result of
    Left (Errors.DriverSessionError _) -> pure ()
    _ -> expectationFailure ("Unexpected pipeline result: " <> show result)
