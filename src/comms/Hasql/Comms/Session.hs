module Hasql.Comms.Session
  ( Session,

    -- * Constructors
    cleanUpAfterInterruption,
    cleanUpAfterFailure,

    -- * Executors
    toHandler,
  )
where

import Data.Text qualified as Text
import Hasql.Comms.Helpers.ConnOps qualified as ConnOps
import Hasql.Comms.Roundtrip qualified as Roundtrip
import Hasql.Platform.Prelude
import Pqi qualified as Pq

-- | Serial execution of commands in the scope of a connection.
newtype Session a = Session (Pq.Connection -> IO (Either Error a))
  deriving
    (Functor, Applicative, Monad, MonadError Error)
    via (ExceptT Error (ReaderT Pq.Connection IO))

type Error = Text

-- * Constructors

-- | Bring the connection to a clean state after an interruption.
--
-- This includes:
-- - Leaving pipeline mode if we are in it.
-- - Bringing the transaction status to idle if we are in a transaction.
-- - Deallocating all prepared statements.
cleanUpAfterInterruption :: Session ()
cleanUpAfterInterruption = do
  drainResults
  cancel
  drainResults
  -- Ensure we are out of pipeline mode.
  leavePipeline
  -- Ensure we are in idle transaction state.
  bringTransactionStatusToIdle
  deallocateAllPreparedStatements

-- | Repair a connection that a session handed back dirty on an ordinary
-- 'Left' return, as opposed to an interruption.
--
-- A 'Left' return means the driver completed its receive sequence and chose
-- to report: nothing is in flight, so there is nothing to cancel; a
-- transaction left open is for whoever composed it (e.g. @hasql-transaction@)
-- to roll back, not this repair; and the prepared-statement cache is still
-- trustworthy, so it is left alone. The only thing a session can leave
-- behind on this path is an open pipeline (see 'leavePipeline'), so that is
-- the only thing this repairs.
cleanUpAfterFailure :: Session ()
cleanUpAfterFailure = leavePipeline

bringTransactionStatusToIdle :: Session ()
bringTransactionStatusToIdle = do
  transactionStatus <- getTransactionStatus
  case transactionStatus of
    Pq.TransIdle -> pure ()
    Pq.TransInTrans -> do
      runCommand "ABORT"
    Pq.TransActive -> do
      -- A command is still in progress.
      drainResults
      -- Check status again after draining.
      transactionStatus <- getTransactionStatus
      case transactionStatus of
        Pq.TransIdle -> pure ()
        Pq.TransInTrans -> do
          runCommand "ABORT"
        Pq.TransActive -> do
          -- If we're still active, there's not much we can do.
          -- The connection is probably in a bad state.
          throwError "Failed to bring transaction status to idle after draining results"
        Pq.TransInError -> do
          runCommand "ABORT"
        Pq.TransUnknown -> do
          -- Unknown state (connection issue), there's not much we can do.
          throwError "Transaction status is unknown, connection is corrupted"
    Pq.TransInError -> do
      -- Transaction is in error state, we need to abort it.
      runCommand "ABORT"
    Pq.TransUnknown -> do
      -- Unknown state (connection issue), there's not much we can do.
      throwError "Transaction status is unknown, connection is corrupted"

-- | PipelineAborted is still pipeline mode. It must reach a sync point
-- before libpq permits serial queries such as ABORT or DEALLOCATE ALL
-- again.
leavePipeline :: Session ()
leavePipeline = Session ConnOps.leave

deallocateAllPreparedStatements :: Session ()
deallocateAllPreparedStatements =
  runCommand "DEALLOCATE ALL"

cancel :: Session ()
cancel = Session \connection -> do
  mCancel <- Pq.getCancel connection
  case mCancel of
    Just cancel -> do
      result <- Pq.cancel cancel
      case result of
        Left errorMessage ->
          pure (Left ("Failed to cancel: " <> decodeUtf8Lenient errorMessage))
        Right () ->
          pure (Right ())
    Nothing -> pure (Right ())

getTransactionStatus :: Session Pq.TransactionStatus
getTransactionStatus = Session \connection -> do
  Right <$> Pq.transactionStatus connection

-- Drain all pending results from the connection.
drainResults :: Session ()
drainResults = Session \connection ->
  Right <$> void (ConnOps.drainProgressively connection)

runCommand :: ByteString -> Session ()
runCommand sql = runRoundtrip (Roundtrip.query () sql)

runRoundtrip :: Roundtrip.Roundtrip () a -> Session a
runRoundtrip roundtrip = Session \connection -> do
  result <- Roundtrip.toSerialIO roundtrip connection
  case result of
    Left err ->
      let message = case err of
            Roundtrip.ClientError () _ details ->
              if Text.null details
                then "Unknown client error occurred"
                else "Client error occurred: " <> details
            Roundtrip.ServerError recvError ->
              "Server error occurred: " <> fromString (show recvError)
       in pure (Left message)
    Right value -> pure (Right value)

-- * Executors

toHandler :: Session a -> Pq.Connection -> IO (Either Text a)
toHandler (Session run) = run
