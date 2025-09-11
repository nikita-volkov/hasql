-- |
-- This module provides a low-level effectful API dealing with the connections to the database.
module Hasql.Connection.Core where

import Hasql.Connection.Config qualified as Config
import Hasql.Connection.Setting qualified as Setting
import Hasql.IO qualified as IO
import Hasql.LibPq14 qualified as Pq
import Hasql.Prelude
import Hasql.PreparedStatementRegistry qualified as PreparedStatementRegistry

-- |
-- A single connection to the database.
data Connection
  = Connection
      -- | Whether prepared statements are allowed.
      !Bool
      -- | Lower level libpq connection.
      !(MVar Pq.Connection)
      -- | Integer datetimes.
      !Bool
      -- | Prepared statement registry.
      !PreparedStatementRegistry.PreparedStatementRegistry

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
    registry <- lift (IO.acquirePreparedStatementRegistry)
    pqConnectionRef <- lift (newMVar pqConnection)
    pure (Connection (Config.usePreparedStatements config) pqConnectionRef integerDatetimes registry)
  where
    config = Config.fromUpdates settings

-- |
-- Release the connection.
release :: Connection -> IO ()
release (Connection _ pqConnectionRef _ _) =
  mask_ $ do
    nullConnection <- Pq.newNullConnection
    pqConnection <- swapMVar pqConnectionRef nullConnection
    IO.releaseConnection pqConnection

-- |
-- Execute an operation on the raw @libpq@ 'Pq.Connection'.
--
-- The access to the connection is exclusive.
withLibPQConnection :: Connection -> (Pq.Connection -> IO a) -> IO a
withLibPQConnection (Connection _ pqConnectionRef _ _) =
  withMVar pqConnectionRef
