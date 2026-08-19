module Integration.Sharing.Session.OnLibpqConnection.DirtyReturnCleanupSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Errors qualified as Errors
import Hasql.Session qualified as Session
import Helpers.Dsls.Execution qualified as Execution
import Helpers.Scripts qualified as Scripts
import Helpers.Statements.SelectOne qualified as Statements.SelectOne
import Pqi qualified as Pq
import Prelude
import Test.Hspec

-- | 'Session.onLibpqConnection' is a low-level escape hatch: whatever state
-- the supplied action leaves the raw libpq connection in is adopted
-- verbatim, on every path, with no check anywhere inside
-- "Hasql.Engine.Contexts.Session" or "Hasql.Comms.Roundtrip". A well-behaved
-- action would never leave the connection mid-pipeline, but nothing stops a
-- buggy one from doing so, and once it has, every subsequent operation on
-- the connection is refused by libpq until something takes it out of
-- pipeline mode.
--
-- Nothing repairs that for the caller. The driver has no way of telling a
-- connection the escape hatch quietly broke from one it left clean, and the
-- one repair it could attempt - pushing a Sync - would flush and commit
-- whatever the caller had queued. So the contract is that the escape hatch
-- restores what it breaks, and reports 'Errors.ConnectionUseError' or
-- 'Errors.DriverUseError' when it cannot.
--
-- These drive the connection into exactly that state directly, entering
-- pipeline mode on the raw handle without leaving it, and pin both halves:
-- the reported case is finished on the spot, and the unreported one cannot
-- poison a pool for long either, because the first operation libpq refuses
-- on it finishes it too.
spec :: SpecWith Scripts.ScopeParams
spec = do
  describe "onLibpqConnection leaving the raw connection in pipeline mode" do
    describe "Reporting a connection error" do
      it "Gets the connection finished rather than repaired" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection (dirtyReturn (Errors.ConnectionUseError "Simulated failure"))
          case result of
            Left (Errors.ConnectionUseError _) -> pure ()
            _ -> expectationFailure ("Unexpected result: " <> show result)

          followUp <-
            Connection.use connection
              $ Execution.sessionByParams Statements.SelectOne.SelectOne
          case followUp of
            Left (Errors.ConnectionUseError _) -> pure ()
            _ -> expectationFailure ("Unexpected follow-up result: " <> show followUp)

    describe "Reporting an error that says nothing about the connection" do
      it "Leaves the connection exactly as the escape hatch left it" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          result <-
            Connection.use connection
              $ dirtyReturn (Errors.SessionUseError (Errors.MissingTypesSessionError mempty))
          case result of
            Left (Errors.SessionUseError (Errors.MissingTypesSessionError _)) -> pure ()
            _ -> expectationFailure ("Unexpected result: " <> show result)

          -- Nothing repaired it: the mode the escape hatch turned on is
          -- still on. That is the whole contract - the driver finishes
          -- connections it has given up on and touches nothing else, so an
          -- error that says nothing about the connection leaves it to the
          -- caller.
          --
          -- Which is survivable here only because an empty pipeline mode
          -- happens to be harmless to hasql's own operations: libpq's
          -- enterPipelineMode succeeds with no action when the mode is
          -- already on, so the next pipeline adopts it and leaves it off
          -- again on the way out. An escape hatch that left commands queued
          -- or results undrained would not be so lucky, which is why the
          -- documented advice is to restore what you break.
          status <-
            Connection.use connection
              $ Session.onLibpqConnection \pqConnection -> do
                status <- Pq.pipelineStatus pqConnection
                pure (Right status, pqConnection)
          status `shouldNotBe` Right Pq.PipelineOff

-- | Enter pipeline mode on the raw connection and report the given error
-- without leaving it.
dirtyReturn :: Errors.UseError -> Session.Session ()
dirtyReturn err =
  Session.onLibpqConnection \pqConnection -> do
    entered <- Pq.enterPipelineMode pqConnection
    entered `shouldBe` True
    pure (Left err, pqConnection)
