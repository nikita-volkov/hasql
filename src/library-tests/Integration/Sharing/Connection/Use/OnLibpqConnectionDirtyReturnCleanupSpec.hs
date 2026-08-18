module Integration.Sharing.Connection.Use.OnLibpqConnectionDirtyReturnCleanupSpec (spec) where

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
-- buggy one (or a genuinely failed one, reporting 'Left') from doing so, and
-- once it has, every subsequent operation on the connection is refused by
-- libpq until something takes it out of pipeline mode.
--
-- This drives the connection into exactly that state directly - entering
-- pipeline mode on the raw handle and reporting a plain 'Left' without
-- leaving it - to exercise the repair 'Hasql.Connection.use' now performs
-- unconditionally on a 'Left' return.
spec :: SpecWith Scripts.ScopeParams
spec = do
  describe "onLibpqConnection leaving the raw connection in pipeline mode on a Left return" do
    it "Connection.use repairs it instead of stranding the connection" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <-
          Connection.use connection
            $ Session.onLibpqConnection @()
            $ \pqConnection -> do
              entered <- Pq.enterPipelineMode pqConnection
              entered `shouldBe` True
              pure (Left (Errors.ConnectionSessionError "Simulated failure with the connection left in pipeline mode"), pqConnection)

        case result of
          Left (Errors.ConnectionSessionError _) -> pure ()
          _ -> expectationFailure ("Unexpected result: " <> show result)

        followUp <-
          Connection.use connection
            $ Execution.sessionByParams Statements.SelectOne.SelectOne

        followUp `shouldBe` Right 1
