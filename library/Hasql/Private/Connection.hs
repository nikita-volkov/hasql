-- |
-- This module provides a low-level effectful API dealing with the connections to the database.
module Hasql.Private.Connection
where

import Hasql.Private.Prelude
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Hasql.Private.PreparedStatementRegistry as PreparedStatementRegistry
import qualified Hasql.Private.IO as IO
import qualified Hasql.Private.Settings as Settings


-- |
-- A single connection to the database.
data Connection =
  Connection !(MVar LibPQ.Connection) !Bool !PreparedStatementRegistry.PreparedStatementRegistry

-- |
-- Possible details of the connection acquistion error.
type ConnectionError =
  Maybe ByteString

-- |
-- Acquire a connection using the provided settings encoded according to the PostgreSQL format.
acquire :: Settings.Settings -> IO (Either ConnectionError Connection)
acquire settings =
  {-# SCC "acquire" #-}
  runExceptT $ do
    pqConnection <- lift (IO.acquireConnection settings)
    lift (IO.checkConnectionStatus pqConnection) >>= traverse throwError
    lift (IO.initConnection pqConnection)
    integerDatetimes <- lift (IO.getIntegerDatetimes pqConnection)
    registry <- lift (IO.acquirePreparedStatementRegistry)
    pqConnectionRef <- lift (newMVar pqConnection)
    pure (Connection pqConnectionRef integerDatetimes registry)

-- |
-- Release the connection.
release :: Connection -> IO ()
release (Connection pqConnectionRef _ _) =
  mask_ $ do
    nullConnection <- LibPQ.newNullConnection
    pqConnection <- swapMVar pqConnectionRef nullConnection
    IO.releaseConnection pqConnection

-- |
-- Execute an operation on the raw @libpq@ 'LibPQ.Connection'.
--
-- The access to the connection is exclusive.
withLibPQConnection :: Connection -> (LibPQ.Connection -> IO a) -> IO a
withLibPQConnection (Connection pqConnectionRef _ _) =
  withMVar pqConnectionRef
