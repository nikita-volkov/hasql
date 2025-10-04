-- |
-- This module provides a low-level effectful API dealing with the connections to the database.
module Hasql.Connection
  ( Connection,
    AcquisitionError,
    UsageError (..),
    acquire,
    release,
    use,
    withLibPQConnection,
  )
where

import Core.Contexts.Session qualified as Session
import Core.Structures.ConnectionState qualified as ConnectionState
import Core.Structures.StatementCache qualified as StatementCache
import Core.UsageError
import Data.Text.Encoding qualified as Text.Encoding
import Hasql.Connection.Config qualified as Config
import Hasql.Connection.PqProcedures qualified as PqProcedures
import Hasql.Connection.Setting qualified as Setting
import Platform.Prelude
import Pq qualified

-- |
-- A single connection to the database.
newtype Connection
  = Connection (MVar ConnectionState.ConnectionState)

-- |
-- Possible details of the connection acquistion error.
type AcquisitionError =
  Maybe ByteString

-- |
-- Establish a connection according to the provided settings.
acquire ::
  [Setting.Setting] ->
  IO (Either AcquisitionError Connection)
acquire settings =
  {-# SCC "acquire" #-}
  runExceptT $ do
    pqConnection <- lift (Pq.connectdb (Config.connectionString config))
    lift (PqProcedures.checkConnectionStatus pqConnection) >>= traverse throwError
    lift (PqProcedures.checkServerVersion pqConnection) >>= traverse (throwError . Just . Text.Encoding.encodeUtf8)
    lift (PqProcedures.initConnection pqConnection)
    let connectionState =
          ConnectionState.ConnectionState
            { ConnectionState.preparedStatements = Config.usePreparedStatements config,
              ConnectionState.statementCache = StatementCache.empty,
              ConnectionState.connection = pqConnection
            }
    connectionRef <- lift (newMVar connectionState)
    pure (Connection connectionRef)
  where
    config = Config.fromUpdates settings

-- |
-- Release the connection.
release :: Connection -> IO ()
release (Connection connectionRef) =
  mask_ do
    connectionState <- readMVar connectionRef
    Pq.finish (ConnectionState.connection connectionState)

-- |
-- Execute an operation on the raw @libpq@ 'Pq.Connection'.
--
-- The access to the connection is exclusive.
{-# DEPRECATED withLibPQConnection "Use @Hasql.Session.'Hasql.Session.onLibpqConnection'@ instead" #-}
withLibPQConnection :: Connection -> (Pq.Connection -> IO a) -> IO a
withLibPQConnection connection action =
  useConnectionState connection \connectionState ->
    (,connectionState)
      <$> action (ConnectionState.connection connectionState)

-- |
-- Execute a sequence of operations with exclusive access to the connection.
--
-- Blocks until the connection is available when there is another session running upon the connection.
use :: Connection -> Session.Session a -> IO (Either UsageError a)
use connection session =
  useConnectionState connection \connectionState -> do
    Session.run session connectionState

useConnectionState :: Connection -> (ConnectionState.ConnectionState -> IO (a, ConnectionState.ConnectionState)) -> IO a
useConnectionState (Connection var) handler =
  mask \restore -> do
    connectionState@ConnectionState.ConnectionState {..} <- takeMVar var
    result <- try @SomeException (restore (handler connectionState))
    case result of
      Left exception -> do
        -- If an exception happened, we need to check the connection status.
        -- If the connection is not idle, we need to reset it
        -- and clear the prepared statement registry.
        Pq.transactionStatus connection >>= \case
          Pq.TransIdle -> do
            -- If the connection is idle, just put back the connection state.
            putMVar var connectionState
          _ -> do
            -- If the connection is not idle, reset the prepared statement registry
            -- and reset the connection itself.
            putMVar var (ConnectionState.resetPreparedStatementsCache connectionState)
            Pq.reset connection

        throwIO exception
      Right (result, !newState) -> do
        putMVar var newState
        pure result
