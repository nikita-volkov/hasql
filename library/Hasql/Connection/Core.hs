-- |
-- This module provides a low-level effectful API dealing with the connections to the database.
module Hasql.Connection.Core where

import Hasql.Connection.Config qualified as Config
import Hasql.Connection.Setting qualified as Setting
import Hasql.IO qualified as IO
import Hasql.LibPq14 qualified as Pq
import Hasql.Prelude
import Hasql.Structures.ConnectionState qualified as ConnectionState
import Hasql.Structures.StatementCache qualified as StatementCache

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
    let connectionState =
          ConnectionState.ConnectionState
            { ConnectionState.preparedStatements = Config.usePreparedStatements config,
              ConnectionState.integerDatetimes = integerDatetimes,
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
  mask_ $ do
    connectionState <- readMVar connectionRef
    IO.releaseConnection (ConnectionState.connection connectionState)

-- |
-- Execute an operation on the raw @libpq@ 'Pq.Connection'.
--
-- The access to the connection is exclusive.
withLibPQConnection :: Connection -> (Pq.Connection -> IO a) -> IO a
withLibPQConnection (Connection connectionRef) action =
  withMVar connectionRef (action . ConnectionState.connection)
