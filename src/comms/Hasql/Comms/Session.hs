module Hasql.Comms.Session
  ( Session,

    -- * Constructors
    cleanUpAfterInterruption,

    -- * Executors
    toHandler,
  )
where

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
      runScript "ABORT"
    Pq.TransActive -> do
      -- A command is still in progress.
      drainResults
      -- Check status again after draining.
      transactionStatus <- getTransactionStatus
      case transactionStatus of
        Pq.TransIdle -> pure ()
        Pq.TransInTrans -> do
          runScript "ABORT"
        Pq.TransActive -> do
          -- If we're still active, there's not much we can do.
          -- The connection is probably in a bad state.
          throwError "Failed to bring transaction status to idle after draining results"
        Pq.TransInError -> do
          runScript "ABORT"
        Pq.TransUnknown -> do
          -- Unknown state (connection issue), there's not much we can do.
          throwError "Transaction status is unknown, connection is corrupted"
    Pq.TransInError -> do
      -- Transaction is in error state, we need to abort it.
      runScript "ABORT"
    Pq.TransUnknown -> do
      -- Unknown state (connection issue), there's not much we can do.
      throwError "Transaction status is unknown, connection is corrupted"

-- | PipelineAborted is still pipeline mode. It must reach a sync point
-- before libpq permits serial queries such as ABORT or DEALLOCATE ALL
-- again.
leavePipeline :: Session ()
leavePipeline = Session Roundtrip.leavePipelineMode

deallocateAllPreparedStatements :: Session ()
deallocateAllPreparedStatements =
  runScript "DEALLOCATE ALL"

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
  let go = do
        mResult <- Pq.getResult connection
        case mResult of
          Nothing -> pure ()
          Just _ -> go
   in go $> Right ()

runScript :: ByteString -> Session ()
runScript script = runRoundtrip (Roundtrip.query () script)

runRoundtrip :: Roundtrip.Roundtrip () a -> Session a
runRoundtrip roundtrip = Session \connection -> do
  result <- Roundtrip.toSerialIO roundtrip connection
  case result of
    Left err ->
      let message = case err of
            Roundtrip.ClientError () _ Nothing ->
              "Unknown client error occurred"
            Roundtrip.ClientError () _ (Just details) ->
              "Client error occurred: " <> decodeUtf8Lenient details
            Roundtrip.ServerError recvError ->
              "Server error occurred: " <> fromString (show recvError)
       in pure (Left message)
    Right value -> pure (Right value)

-- * Executors

toHandler :: Session a -> Pq.Connection -> IO (Either Text a)
toHandler (Session run) = run
