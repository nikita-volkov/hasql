module Hasql.Comms.Session
  ( Session,

    -- * Constructors
    cleanUpAfterInterruption,

    -- * Executors
    toHandler,
  )
where

import Hasql.Comms.Roundtrip qualified as Roundtrip
import Hasql.Driver.Interface qualified as Interface
import Hasql.Platform.Prelude

-- | Serial execution of commands in the scope of a connection.
newtype Session conn a = Session (conn -> IO (Either Error a))
  deriving
    (Functor, Applicative, Monad, MonadError Error)
    via (ExceptT Error (ReaderT conn IO))

type Error = Text

-- * Constructors

-- | Bring the connection to a clean state after an interruption.
--
-- This includes:
-- - Leaving pipeline mode if we are in it.
-- - Bringing the transaction status to idle if we are in a transaction.
-- - Deallocating all prepared statements.
cleanUpAfterInterruption :: Interface.Driver conn result -> Session conn ()
cleanUpAfterInterruption drv = do
  drainResults drv
  cancel drv
  drainResults drv
  -- Ensure we are out of pipeline mode.
  leavePipeline drv
  -- Ensure we are in idle transaction state.
  bringTransactionStatusToIdle drv
  deallocateAllPreparedStatements drv

bringTransactionStatusToIdle :: Interface.Driver conn result -> Session conn ()
bringTransactionStatusToIdle drv = do
  transactionStatus <- getTransactionStatus drv
  case transactionStatus of
    Interface.TransIdle -> pure ()
    Interface.TransInTrans -> do
      runScript drv "ABORT"
    Interface.TransActive -> do
      -- A command is still in progress.
      drainResults drv
      -- Check status again after draining.
      transactionStatus <- getTransactionStatus drv
      case transactionStatus of
        Interface.TransIdle -> pure ()
        Interface.TransInTrans -> do
          runScript drv "ABORT"
        Interface.TransActive -> do
          -- If we're still active, there's not much we can do.
          -- The connection is probably in a bad state.
          throwError "Failed to bring transaction status to idle after draining results"
        Interface.TransInError -> do
          runScript drv "ABORT"
        Interface.TransUnknown -> do
          -- Unknown state (connection issue), there's not much we can do.
          throwError "Transaction status is unknown, connection is corrupted"
    Interface.TransInError -> do
      -- Transaction is in error state, we need to abort it.
      runScript drv "ABORT"
    Interface.TransUnknown -> do
      -- Unknown state (connection issue), there's not much we can do.
      throwError "Transaction status is unknown, connection is corrupted"

leavePipeline :: Interface.Driver conn result -> Session conn ()
leavePipeline drv = do
  pipelineStatus <- getPipelineStatus drv
  when (pipelineStatus == Interface.PipelineOn) do
    -- In pipeline mode, we need to ensure the pipeline is synchronized before exiting.
    -- Send a pipeline sync marker to flush any pending operations.
    syncSuccess <- sendPipelineSync drv
    when syncSuccess (drainResults drv)
    -- After sync, send a flush to ensure all queued commands are sent to the server.
    flushSuccess <- sendFlushRequest drv
    when flushSuccess (drainResults drv)
    -- Try to exit pipeline mode.
    -- This might fail if there are pending results that need to be consumed.
    success <- exitPipelineMode drv
    unless success do
      -- If exit failed, drain results and try again.
      drainResults drv
      success <- exitPipelineMode drv
      unless success do
        -- If it still fails, there's not much we can do.
        -- The connection is probably in a bad state.
        errorMessage <- getErrorMessage drv
        let message = case errorMessage of
              Nothing -> "Failed to exit pipeline mode after draining results"
              Just details -> "Failed to exit pipeline mode after draining results: " <> decodeUtf8Lenient details
        throwError message

deallocateAllPreparedStatements :: Interface.Driver conn result -> Session conn ()
deallocateAllPreparedStatements drv =
  runScript drv "DEALLOCATE ALL"

cancel :: Interface.Driver conn result -> Session conn ()
cancel drv = Session \connection -> do
  result <- Interface.driverCancel drv connection
  case result of
    Left errorMessage ->
      pure (Left ("Failed to cancel: " <> decodeUtf8Lenient errorMessage))
    Right () ->
      pure (Right ())

getErrorMessage :: Interface.Driver conn result -> Session conn (Maybe ByteString)
getErrorMessage drv = Session \connection -> do
  Right <$> Interface.driverErrorMessage drv connection

getTransactionStatus :: Interface.Driver conn result -> Session conn Interface.TransactionStatus
getTransactionStatus drv = Session \connection -> do
  Right <$> Interface.driverTransactionStatus drv connection

getPipelineStatus :: Interface.Driver conn result -> Session conn Interface.PipelineStatus
getPipelineStatus drv = Session \connection -> do
  Right <$> Interface.driverPipelineStatus drv connection

exitPipelineMode :: Interface.Driver conn result -> Session conn Bool
exitPipelineMode drv = Session \connection -> do
  Right <$> Interface.driverExitPipelineMode drv connection

sendPipelineSync :: Interface.Driver conn result -> Session conn Bool
sendPipelineSync drv = Session \connection -> do
  Right <$> Interface.driverPipelineSync drv connection

sendFlushRequest :: Interface.Driver conn result -> Session conn Bool
sendFlushRequest drv = Session \connection -> do
  Right <$> Interface.driverSendFlushRequest drv connection

-- Drain all pending results from the connection.
drainResults :: Interface.Driver conn result -> Session conn ()
drainResults drv = Session \connection ->
  let go = do
        mResult <- Interface.driverGetResult drv connection
        case mResult of
          Nothing -> pure ()
          Just _ -> go
   in go $> Right ()

runScript :: Interface.Driver conn result -> ByteString -> Session conn ()
runScript drv sql = runRoundtrip drv (Roundtrip.query drv () sql)

runRoundtrip :: Interface.Driver conn result -> Roundtrip.Roundtrip conn () a -> Session conn a
runRoundtrip _drv roundtrip = Session \connection -> do
  result <- Roundtrip.toSerialIO roundtrip connection
  case result of
    Left err ->
      let message = case err of
            Roundtrip.ClientError () Nothing ->
              "Unknown client error occurred"
            Roundtrip.ClientError () (Just details) ->
              "Client error occurred: " <> decodeUtf8Lenient details
            Roundtrip.ServerError recvError ->
              "Server error occurred: " <> fromString (show recvError)
       in pure (Left message)
    Right value -> pure (Right value)

-- * Executors

toHandler :: Session conn a -> conn -> IO (Either Text a)
toHandler (Session run) = run
