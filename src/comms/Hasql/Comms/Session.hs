module Hasql.Comms.Session
  ( Session,

    -- * Constructors
    cleanUpAfterInterruption,

    -- * Executors
    toHandler,
  )
where

import Data.Text qualified as Text
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

-- | Leave the pipeline mode of a connection, draining all the results that
-- the commands dispatched in it have produced or are still producing.
--
-- A connection in pipeline mode cannot serve serial commands: libpq rejects
-- them while the mode is on, and the server withholds the results of the
-- dispatched commands until it receives a Sync. Hence before turning the
-- mode off we send a Sync and a Flush and drain everything that comes back.
--
-- Draining is not a single pass: in pipeline mode @PQgetResult@ terminates
-- the round of results of every command with a 'Nothing', so a drain loop
-- stops at each command boundary. 'Pq.exitPipelineMode' only succeeds once
-- the command queue is empty, so draining and exit attempts alternate for as
-- long as draining keeps making progress.
--
-- Idempotent: a no-op when the connection is not in pipeline mode, costing
-- one local @PQpipelineStatus@ call and no network traffic. PipelineAborted
-- is still pipeline mode, and it must reach a sync point before libpq
-- permits serial queries such as ABORT or DEALLOCATE ALL again.
leavePipeline :: Session ()
leavePipeline = Session \connection -> do
  pipelineStatus <- Pq.pipelineStatus connection
  if pipelineStatus == Pq.PipelineOff
    then pure (Right ())
    else do
      _ <- Pq.pipelineSync connection
      void (drainProgressively connection)
      -- Ensure any queued commands the Sync above didn't flush on its own
      -- reach the server before we start waiting on results for them.
      _ <- Pq.sendFlushRequest connection
      void (drainProgressively connection)
      exitWithDraining connection
  where
    exitWithDraining connection =
      Pq.exitPipelineMode connection >>= \case
        True -> pure (Right ())
        False ->
          drainProgressively connection >>= \case
            True -> exitWithDraining connection
            False -> do
              errorMessage <- Pq.errorMessage connection
              pure (Left (maybe "" decodeUtf8Lenient errorMessage))

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
  Right <$> void (drainProgressively connection)

-- | Consume the results of the currently dispatched commands, reporting
-- whether anything got consumed.
--
-- In pipeline mode this drains up to the next command boundary, since every
-- command's round of results is terminated by a 'Nothing'.
drainProgressively :: Pq.Connection -> IO Bool
drainProgressively connection =
  let go hasConsumedResult =
        Pq.getResult connection >>= \case
          Just _ -> go True
          Nothing -> pure hasConsumedResult
   in go False

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
