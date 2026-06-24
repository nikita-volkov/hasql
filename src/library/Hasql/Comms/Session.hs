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
import Pqi qualified

-- | Serial execution of commands in the scope of a connection.
newtype Session a
  = Session (forall conn. (Pqi.IsConnection conn) => conn -> IO (Either Error a))

instance Functor Session where
  fmap f (Session g) = Session $ \c -> fmap (fmap f) (g c)

instance Applicative Session where
  pure a = Session $ \_ -> pure (Right a)
  Session f <*> Session x = Session $ \c -> (<*>) <$> f c <*> x c

instance Monad Session where
  Session x >>= f = Session $ \c ->
    x c >>= either (pure . Left) (\a -> let Session g = f a in g c)

instance MonadError Error Session where
  throwError e = Session $ \_ -> pure (Left e)
  catchError (Session x) handler = Session $ \c ->
    x c >>= either (\e -> let Session g = handler e in g c) (pure . Right)

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
    Pqi.TransIdle -> pure ()
    Pqi.TransInTrans -> do
      runScript "ABORT"
    Pqi.TransActive -> do
      -- A command is still in progress.
      drainResults
      -- Check status again after draining.
      transactionStatus <- getTransactionStatus
      case transactionStatus of
        Pqi.TransIdle -> pure ()
        Pqi.TransInTrans -> do
          runScript "ABORT"
        Pqi.TransActive -> do
          -- If we're still active, there's not much we can do.
          -- The connection is probably in a bad state.
          throwError "Failed to bring transaction status to idle after draining results"
        Pqi.TransInError -> do
          runScript "ABORT"
        Pqi.TransUnknown -> do
          -- Unknown state (connection issue), there's not much we can do.
          throwError "Transaction status is unknown, connection is corrupted"
    Pqi.TransInError -> do
      -- Transaction is in error state, we need to abort it.
      runScript "ABORT"
    Pqi.TransUnknown -> do
      -- Unknown state (connection issue), there's not much we can do.
      throwError "Transaction status is unknown, connection is corrupted"

leavePipeline :: Session ()
leavePipeline = do
  pipelineStatus <- getPipelineStatus
  when (pipelineStatus == Pqi.PipelineOn) do
    -- In pipeline mode, we need to ensure the pipeline is synchronized before exiting.
    -- Send a pipeline sync marker to flush any pending operations.
    syncSuccess <- sendPipelineSync
    when syncSuccess drainResults
    -- After sync, send a flush to ensure all queued commands are sent to the server.
    flushSuccess <- sendFlushRequest
    when flushSuccess drainResults
    -- Try to exit pipeline mode.
    -- This might fail if there are pending results that need to be consumed.
    success <- exitPipelineMode
    unless success do
      -- If exit failed, drain results and try again.
      drainResults
      success <- exitPipelineMode
      unless success do
        -- If it still fails, there's not much we can do.
        -- The connection is probably in a bad state.
        errorMessage <- getErrorMessage
        let message = case errorMessage of
              Nothing -> "Failed to exit pipeline mode after draining results"
              Just details -> "Failed to exit pipeline mode after draining results: " <> decodeUtf8Lenient details
        throwError message

deallocateAllPreparedStatements :: Session ()
deallocateAllPreparedStatements =
  runScript "DEALLOCATE ALL"

cancel :: Session ()
cancel = Session \connection -> do
  mCancel <- Pqi.getCancel connection
  case mCancel of
    Just cancelHandle -> do
      result <- Pqi.cancel cancelHandle
      case result of
        Left errorMessage ->
          pure (Left ("Failed to cancel: " <> decodeUtf8Lenient errorMessage))
        Right () ->
          pure (Right ())
    Nothing -> pure (Right ())

getErrorMessage :: Session (Maybe ByteString)
getErrorMessage = Session \connection -> do
  Right <$> Pqi.errorMessage connection

getTransactionStatus :: Session Pqi.TransactionStatus
getTransactionStatus = Session \connection -> do
  Right <$> Pqi.transactionStatus connection

getPipelineStatus :: Session Pqi.PipelineStatus
getPipelineStatus = Session \connection -> do
  Right <$> Pqi.pipelineStatus connection

exitPipelineMode :: Session Bool
exitPipelineMode = Session \connection -> do
  Right <$> Pqi.exitPipelineMode connection

sendPipelineSync :: Session Bool
sendPipelineSync = Session \connection -> do
  Right <$> Pqi.pipelineSync connection

sendFlushRequest :: Session Bool
sendFlushRequest = Session \connection -> do
  Right <$> Pqi.sendFlushRequest connection

-- Drain all pending results from the connection.
drainResults :: Session ()
drainResults = Session \connection ->
  let go = do
        mResult <- Pqi.getResult connection
        case mResult of
          Nothing -> pure ()
          Just _ -> go
   in go $> Right ()

runScript :: ByteString -> Session ()
runScript sql = runRoundtrip (Roundtrip.query () sql)

runRoundtrip :: Roundtrip.Roundtrip () a -> Session a
runRoundtrip roundtrip = Session \connection -> do
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

toHandler :: (Pqi.IsConnection conn) => Session a -> conn -> IO (Either Text a)
toHandler (Session run) = run
