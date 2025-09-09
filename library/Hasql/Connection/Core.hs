-- |
-- This module provides a low-level effectful API dealing with the connections to the database.
module Hasql.Connection.Core where

import Hasql.Connection.Config qualified as Config
import Hasql.Connection.Setting qualified as Setting
import Hasql.IO qualified as IO
import Hasql.LibPq14 qualified as LibPQ
import Hasql.Prelude
import Hasql.PreparedStatementRegistry qualified as PreparedStatementRegistry
import Hasql.Structures.ConnectionState qualified as ConnectionState
import Hasql.Structures.RegistryState qualified as Map

-- |
-- A single connection to the database.
newtype Connection
  = Connection (MVar ConnectionState.ConnectionState)

-- |
-- Possible details of the connection acquistion error.
type ConnectionError =
  Maybe ByteString

-- |
-- Establish a connection according to the provided settings.
acquire ::
  [Setting.Setting] ->
  IO (Either ConnectionError Connection)
acquire settings =
  {-# SCC "acquire" #-}
  runExceptT $ do
    pqConnection <- lift (IO.acquireConnection (Config.connectionString config))
    lift (IO.checkConnectionStatus pqConnection) >>= traverse throwError
    lift (IO.initConnection pqConnection)
    integerDatetimes <- lift (IO.getIntegerDatetimes pqConnection)
    let registryState = Map.empty  -- Start with empty registry state
    let connectionState = ConnectionState.ConnectionState 
          (Config.usePreparedStatements config) 
          pqConnection 
          integerDatetimes 
          registryState
    connectionStateRef <- lift (newMVar connectionState)
    pure (Connection connectionStateRef)
  where
    config = Config.fromUpdates settings

-- |
-- Release the connection.
release :: Connection -> IO ()
release (Connection connectionStateRef) =
  mask_ $ do
    ConnectionState.ConnectionState _ pqConnection _ _ <- readMVar connectionStateRef
    nullConnection <- LibPQ.newNullConnection
    _ <- swapMVar connectionStateRef (ConnectionState.ConnectionState False nullConnection False Map.empty)
    IO.releaseConnection pqConnection

-- |
-- Execute an operation on the raw @libpq@ 'LibPQ.Connection'.
--
-- The access to the connection is exclusive.
withLibPQConnection :: Connection -> (LibPQ.Connection -> IO a) -> IO a
withLibPQConnection (Connection connectionStateRef) action =
  withMVar connectionStateRef $ \(ConnectionState.ConnectionState _ pqConnection _ _) -> 
    action pqConnection
