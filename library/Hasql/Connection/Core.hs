-- |
-- This module provides a low-level effectful API dealing with the connections to the database.
module Hasql.Connection.Core where

import Hasql.ConnectionString qualified as ConnectionString
import Hasql.IO qualified as IO
import Hasql.LibPq14 qualified as LibPQ
import Hasql.Prelude
import Hasql.PreparedStatementRegistry qualified as PreparedStatementRegistry

-- |
-- A single connection to the database.
data Connection
  = Connection
      -- | Whether prepared statements are allowed.
      !Bool
      -- | Lower level libpq connection.
      !(MVar LibPQ.Connection)
      -- | Integer datetimes.
      !Bool
      -- | Prepared statement registry.
      !PreparedStatementRegistry.PreparedStatementRegistry

-- |
-- Possible details of the connection acquistion error.
type ConnectionError =
  Maybe ByteString

-- |
-- Acquire a connection using the provided settings encoded according to the PostgreSQL format.
acquire ::
  -- | Whether prepared statements are allowed.
  --
  -- When 'False', even the statements marked as preparable will be executed without preparation.
  --
  -- This is useful when dealing with proxying applications like @pgbouncer@, which may be incompatible with prepared statements.
  -- Consult their docs or just set it to 'False' to stay on the safe side.
  -- It should be noted that starting from version @1.21.0@ @pgbouncer@ now does provide support for prepared statements.
  Bool ->
  ConnectionString.ConnectionString ->
  IO (Either ConnectionError Connection)
acquire usePreparedStatements connectionString =
  {-# SCC "acquire" #-}
  runExceptT $ do
    pqConnection <- lift (IO.acquireConnection connectionString)
    lift (IO.checkConnectionStatus pqConnection) >>= traverse throwError
    lift (IO.initConnection pqConnection)
    integerDatetimes <- lift (IO.getIntegerDatetimes pqConnection)
    registry <- lift (IO.acquirePreparedStatementRegistry)
    pqConnectionRef <- lift (newMVar pqConnection)
    pure (Connection usePreparedStatements pqConnectionRef integerDatetimes registry)

-- |
-- Release the connection.
release :: Connection -> IO ()
release (Connection _ pqConnectionRef _ _) =
  mask_ $ do
    nullConnection <- LibPQ.newNullConnection
    pqConnection <- swapMVar pqConnectionRef nullConnection
    IO.releaseConnection pqConnection

-- |
-- Execute an operation on the raw @libpq@ 'LibPQ.Connection'.
--
-- The access to the connection is exclusive.
withLibPQConnection :: Connection -> (LibPQ.Connection -> IO a) -> IO a
withLibPQConnection (Connection _ pqConnectionRef _ _) =
  withMVar pqConnectionRef
