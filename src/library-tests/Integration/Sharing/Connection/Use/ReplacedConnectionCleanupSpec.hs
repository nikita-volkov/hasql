-- | 'Hasql.Connection.use' cleaning up the connection a session started with
-- rather than the one it left behind.
--
-- 'Hasql.Session.onLibpqConnection' lets a session swap the libpq connection
-- out from under the driver. When such a session is then interrupted by an
-- exception, 'use' has two connections to choose between for its
-- @cleanUpAfterInterruption@ call: the one it took out of the @MVar@ before
-- the session ran, and the one carried by the state the session got as far
-- as. It cleans up the former while storing the latter back in the @MVar@, so
-- the connection that goes on to serve the next session is the one that was
-- never brought back to a clean state.
--
-- Filed under "Sharing" rather than "Isolated": the replacement connection it
-- opens is its own, but everything it does happens on that connection alone,
-- so a shared container is unaffected.
module Integration.Sharing.Connection.Use.ReplacedConnectionCleanupSpec (spec) where

import Data.IORef
import Data.Text.Encoding (encodeUtf8)
import Hasql.Connection qualified as Connection
import Hasql.Session qualified as Session
import Helpers.Dsls.Execution qualified as Execution
import Helpers.Scripts qualified as Scripts
import Helpers.Statements.SelectOne qualified as Statements.SelectOne
import Pqi qualified
import Prelude
import Test.Hspec
import TextBuilder qualified

spec :: SpecWith Scripts.ScopeParams
spec = do
  it "Brings the connection a session left behind back to a clean state" \config@(adapter, host, port) ->
    Scripts.onPreparableConnection config \connection -> do
      -- The connection the session displaces. Ownership of it passes to us
      -- the moment `onLibpqConnection` returns a different one, so we have to
      -- finish it ourselves; the replacement is finished by the `Connection`
      -- bracket above, since that is what the driver holds by then.
      displacedRef <- newIORef Nothing
      -- The replacement's transaction status as the session hands it over,
      -- recorded rather than asserted in place so that a failed expectation
      -- doesn't masquerade as the interrupting exception.
      handedOverStatusRef <- newIORef Nothing

      flip finally (readIORef displacedRef >>= traverse_ Pqi.finish) do
        _ <- try @SomeException do
          Connection.use connection do
            Session.onLibpqConnection \displaced -> do
              replacement <- Pqi.connectdb adapter (connectionString host port)
              -- Leave the replacement inside an aborted transaction - a state
              -- `cleanUpAfterInterruption` exists to ABORT out of.
              _ <- Pqi.exec replacement "begin"
              _ <- Pqi.exec replacement "select 1 / 0"
              writeIORef handedOverStatusRef . Just =<< Pqi.transactionStatus replacement
              writeIORef displacedRef (Just displaced)
              pure (Right (), replacement)
            liftIO (throwIO (userError "Simulated failure"))

        handedOverStatus <- readIORef handedOverStatusRef
        handedOverStatus `shouldBe` Just Pqi.TransInError

        -- The driver now serves sessions off the replacement. Had `use`
        -- cleaned that one up instead of the displaced one, the aborted
        -- transaction would be gone and this would go through.
        result <- Connection.use connection (Execution.sessionByParams Statements.SelectOne.SelectOne)
        result `shouldBe` Right 1

-- | A libpq connection string for the shared container, for opening a raw
-- connection alongside the driver's own.
connectionString :: Text -> Word16 -> ByteString
connectionString host port =
  (encodeUtf8 . TextBuilder.toText . mconcat)
    [ "host=",
      TextBuilder.text host,
      " port=",
      TextBuilder.decimal port,
      " user=postgres password=postgres dbname=postgres"
    ]
