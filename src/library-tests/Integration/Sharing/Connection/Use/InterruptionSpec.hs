-- | What 'Hasql.Connection.use' does when an exception cuts a session short -
-- one the session threw itself, or one delivered from another thread by
-- 'timeout' or 'killThread'.
--
-- The exception propagates, and the connection is finished on the way out.
-- Nothing is repaired: an exception lands somewhere inside a round trip and
-- the driver has no way of finding out where, while the repair it could
-- attempt - draining results, aborting the transaction - is blocking network
-- IO performed under a mask, which on a connection whose peer has gone away
-- never returns. The interruption the caller asked for would then never land.
--
-- So the handle is spent from then on, exactly as it is after a failure the
-- driver could not vouch for the connection through
-- (see "Integration.Sharing.Pipeline.Statement.SendFailureSpec"), and
-- whatever the interrupted session had going on the server is rolled back
-- with the connection rather than left holding locks.
module Integration.Sharing.Connection.Use.InterruptionSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Errors qualified as Errors
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Dsls.Execution qualified as Execution
import Helpers.Scripts qualified as Scripts
import Helpers.Statements.Sleep qualified as Statements
import Prelude
import Test.Hspec

spec :: SpecWith Scripts.ScopeParams
spec = do
  describe "An exception thrown by the session" do
    it "Propagates as itself rather than as a driver wrapper around it" \config ->
      Scripts.onPreparableConnection config \connection -> do
        -- The state a session got as far as reaches `use` on the back of an
        -- exception, so `use` has to unwrap that carrier before re-throwing.
        -- Caught here at the concrete type the session threw: were the
        -- carrier to escape instead, `try` would not match it and it would
        -- fail the example by propagating.
        result <- try @IOException do
          Connection.use connection do
            Session.script "select 1"
            liftIO (throwIO (userError "Original exception"))

        case result of
          Left err -> show err `shouldContain` "Original exception"
          Right _ -> expectationFailure "Expected the exception to propagate out of `use`"

    it "Spends the handle" \config ->
      Scripts.onPreparableConnection config \connection -> do
        interruptWith connection
        shouldReportTheConnectionAsGone connection

    it "Keeps reporting the spent handle as transient, which is a claim about a new connection" \config ->
      -- 'Errors.isTransient' means "retrying against a clean connection state
      -- will succeed", and a spent handle never carries one again. So this
      -- stays 'True' however many times it is asked, and a retry wrapper that
      -- loops on it while reusing the same handle spins rather than
      -- recovering. Pinned here because the promise is made in the haddock on
      -- 'Errors.isTransient' and on 'Errors.ConnectionSessionError', and a
      -- promise with no example is one that drifts.
      Scripts.onPreparableConnection config \connection -> do
        interruptWith connection
        replicateM_ 3 do
          result <- Connection.use connection (Session.script "select 1")
          case result of
            Left err -> Errors.isTransient err `shouldBe` True
            Right () -> expectationFailure "Expected the spent handle to keep failing"

    it "Leaves the connection releasable" \config ->
      -- 'release' on a connection the driver already finished must not
      -- finish it a second time - libpq forbids touching a connection after
      -- PQfinish.
      Scripts.onPreparableConnection config \connection -> do
        interruptWith connection
        Connection.release connection
        Connection.release connection

  describe "Timing out" do
    describe "On a statement" do
      it "Reports the interruption and spends the handle" \config ->
        Scripts.onPreparableConnection config \connection -> do
          shouldTimeOut connection (Execution.sessionByParams (Statements.Sleep 0.1))
          shouldReportTheConnectionAsGone connection

    describe "On a transaction" do
      it "Reports the interruption and spends the handle" \config ->
        Scripts.onPreparableConnection config \connection -> do
          shouldTimeOut connection do
            Session.script "begin;"
            Execution.sessionByParams (Statements.Sleep 0.1)
            Session.script "commit;"
          shouldReportTheConnectionAsGone connection

    describe "In a pipeline" do
      it "Reports the interruption and spends the handle" \config ->
        Scripts.onPreparableConnection config \connection -> do
          shouldTimeOut connection do
            Session.pipeline
              $ Execution.pipelineByParams (Statements.Sleep 0.1)
              *> Execution.pipelineByParams (Statements.Sleep 0.1)
          shouldReportTheConnectionAsGone connection

    describe "On a statement that has never been prepared on this connection" do
      it "Reports the interruption and spends the handle" \config ->
        -- Kept apart from the case above because of what it used to guard.
        -- The driver reacted to an interruption by deallocating every
        -- prepared statement on the server and emptying the local cache to
        -- match, and a statement interrupted before it reached the cache
        -- made the two disagree - the next session got "prepared statement
        -- ... does not exist". Nothing can diverge now: the cache is part of
        -- the connection state, and the connection is discarded whole.
        Scripts.onPreparableConnection config \connection -> do
          shouldTimeOut connection (Session.statement 0.1 sleepStatement)
          shouldReportTheConnectionAsGone connection

  describe "Effects on the database state" do
    it "Rolls back the transaction the interrupted session was in" \config ->
      -- Finishing the connection is what discards the work: the server rolls
      -- the transaction back when the socket goes, so nothing the session had
      -- written survives and no backend is left idle in a transaction holding
      -- its locks.
      --
      -- Observing that takes a second connection, both because the one the
      -- session ran on is finished afterwards and because the effect has to
      -- outlive it.
      Scripts.onUnpreparableConnection config \observer -> do
        tableName <- Scripts.generateSymname
        createResult <-
          Connection.use observer (Session.script ("create table " <> tableName <> " (id int4)"))
        shouldBe createResult (Right ())

        Scripts.onUnpreparableConnection config \connection -> do
          _ <- try @SomeException do
            Connection.use connection do
              Session.script "begin;"
              Session.statement () (insertStatement tableName)
              liftIO (throwIO (userError "Simulated failure"))
          shouldReportTheConnectionAsGone connection

        countResult <- Connection.use observer (Session.statement () (countStatement tableName))
        shouldBe countResult (Right 0)

        dropResult <- Connection.use observer (Session.script ("drop table " <> tableName))
        shouldBe dropResult (Right ())

-- | Run a session that throws partway through, swallowing the exception it
-- propagates.
interruptWith :: Connection.Connection -> IO ()
interruptWith connection = do
  _ <- try @SomeException do
    Connection.use connection do
      Session.script "select 1"
      liftIO (throwIO (userError "Simulated failure"))
  pure ()

-- | Run the session under a timeout tuned to fire inside it, and assert that
-- the interruption came back out of 'Connection.use' as an exception.
--
-- 'Nothing' is what says so: had `use` reported the interruption as a
-- returned error instead, 'timeout' would have observed an ordinary return
-- and given us a 'Just'.
shouldTimeOut :: Connection.Connection -> Session.Session () -> IO ()
shouldTimeOut connection session = do
  result <- timeout 50_000 (Connection.use connection session)
  shouldBe result Nothing

-- | Assert that the handle no longer reaches a connection.
shouldReportTheConnectionAsGone :: Connection.Connection -> IO ()
shouldReportTheConnectionAsGone connection = do
  result <- Connection.use connection (Session.script "select 1")
  case result of
    Left (Errors.ConnectionSessionError _) -> pure ()
    _ -> expectationFailure ("Unexpected follow-up result: " <> show result)

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

-- | Sleeps for the given number of seconds, giving an interruption a window
-- to land in.
sleepStatement :: Statement.Statement Double ()
sleepStatement =
  Statement.preparable
    "select pg_sleep($1)"
    (Encoders.param (Encoders.nonNullable Encoders.float8))
    Decoders.noResult
