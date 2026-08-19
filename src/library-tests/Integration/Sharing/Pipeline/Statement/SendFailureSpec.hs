module Integration.Sharing.Pipeline.Statement.SendFailureSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Errors qualified as Errors
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Dsls.Execution qualified as Execution
import Helpers.Scripts qualified as Scripts
import Helpers.Statements qualified as Statements
import Prelude
import Test.Hspec

-- | A pipeline of five statements with 'Statements.TooManyParams' in the
-- middle: statements 1 and 2 are queued, statement 3 is refused on send, and
-- statements 4 and 5 are never even queued.
--
-- This is the reproduction of <https://github.com/nikita-volkov/hasql/issues/326>.
--
-- Note what "queued" means here and what it does not. In pipeline mode
-- @PQsendQueryParams@ only appends to libpq's output buffer; libpq flushes
-- when the buffer passes a size threshold, and otherwise nothing reaches the
-- server until a Sync. Two @generate_series@ calls are nowhere near that
-- threshold, so at the moment statement 3 is refused, __nothing has left the
-- client__. The driver does not push a Sync to find out - doing so would
-- flush and commit statements 1 and 2, which is the opposite of what a
-- pipeline that failed halfway should do. It finishes the connection
-- instead, and the whole pipeline is discarded, exactly as it would be had
-- the server rejected one of the statements.
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

-- | The same failure with nothing queued before it: the send of the very
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
        -- the same request will be refused the same way on any other
        -- connection. Reporting this as transient is what made retry
        -- wrappers spin on it forever.
        result <- Connection.use connection (Session.pipeline failingPipeline)
        case result of
          Left err -> Errors.isTransient err `shouldBe` False
          Right () -> expectationFailure "The pipeline unexpectedly succeeded"

    it "Finishes the connection" \config -> do
      -- The driver cannot vouch for the protocol state of a connection it
      -- failed to send on: the mode is still on, commands are queued behind
      -- the refusal, and only a Sync would reveal what the server has seen
      -- - at the cost of committing them. So the connection is finished
      -- rather than repaired, and the handle it was reached through is
      -- spent.
      Scripts.onUnpreparableConnection config \connection -> do
        runFailingPipeline connection
        followUpResult <-
          Connection.use connection
            $ Session.script "select 1"
        case followUpResult of
          Left (Errors.ConnectionUseError _) -> pure ()
          _ -> expectationFailure ("Unexpected follow-up result: " <> show followUpResult)

    it "Keeps reporting the connection as gone however many times it is used" \config -> do
      Scripts.onUnpreparableConnection config \connection -> do
        runFailingPipeline connection
        replicateM_ 3 do
          result <- Connection.use connection (Session.script "select 1")
          case result of
            Left (Errors.ConnectionUseError _) -> pure ()
            _ -> expectationFailure ("Unexpected follow-up result: " <> show result)

    it "Leaves the connection releasable" \config -> do
      -- 'release' on a connection the driver already finished must not
      -- finish it a second time - libpq forbids touching a connection after
      -- PQfinish.
      Scripts.onUnpreparableConnection config \connection -> do
        runFailingPipeline connection
        Connection.release connection
        Connection.release connection

    describe "When it happens on the first statement of the pipeline" do
      it "Fails the same way" \config -> do
        -- Nothing was queued before the failure - the opposite end of the
        -- range from the mid-pipeline case above, and the same outcome.
        Scripts.onUnpreparableConnection config \connection -> do
          result <-
            Connection.use connection
              $ Session.pipeline failingOnFirstStatementPipeline
          case result of
            Left (Errors.DriverUseError _) -> pure ()
            _ -> expectationFailure ("Unexpected pipeline result: " <> show result)
          followUpResult <-
            Connection.use connection
              $ Session.script "select 1"
          case followUpResult of
            Left (Errors.ConnectionUseError _) -> pure ()
            _ -> expectationFailure ("Unexpected follow-up result: " <> show followUpResult)

    describe "Effects on the database state" do
      it "Discards the whole pipeline, including the statements preceding the failure" \config -> do
        -- The statements before the failure were appended to libpq's output
        -- buffer and never flushed, so the server never saw them. This
        -- matches what a query error in a pipeline already does: the
        -- implicit transaction block is rolled back whole.
        --
        -- Observing that takes a second connection, both because the one
        -- the pipeline ran on is finished afterwards and because the effect
        -- has to outlive it.
        Scripts.onUnpreparableConnection config \observer -> do
          tableName <- Scripts.generateSymname
          createResult <-
            Connection.use observer (Session.script ("create table " <> tableName <> " (id int4)"))
          shouldBe createResult (Right ())

          pipelineResult <-
            Scripts.onUnpreparableConnection config \connection ->
              (Connection.use connection . Session.pipeline)
                $ (,)
                <$> Pipeline.statement () (insertStatement tableName)
                <*> Execution.pipelineByParams Statements.TooManyParams
          case pipelineResult of
            Left (Errors.DriverUseError _) -> pure ()
            _ -> expectationFailure ("Unexpected pipeline result: " <> show pipelineResult)

          countResult <- Connection.use observer (Session.statement () (countStatement tableName))
          shouldBe countResult (Right 0)

          dropResult <- Connection.use observer (Session.script ("drop table " <> tableName))
          shouldBe dropResult (Right ())

insertStatement :: Text -> Statement.Statement () ()
insertStatement tableName =
  Statement.unpreparable
    ("insert into " <> tableName <> " values (1)")
    mempty
    Decoders.noResult

countStatement :: Text -> Statement.Statement () Int64
countStatement tableName =
  Statement.unpreparable
    ("select count(*) from " <> tableName)
    mempty
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))

-- | Run 'failingPipeline' and assert it fails with the expected rejection.
runFailingPipeline :: Connection.Connection -> IO ()
runFailingPipeline connection = do
  result <-
    Connection.use connection
      $ Session.pipeline failingPipeline
  case result of
    Left (Errors.DriverUseError _) -> pure ()
    _ -> expectationFailure ("Unexpected pipeline result: " <> show result)
