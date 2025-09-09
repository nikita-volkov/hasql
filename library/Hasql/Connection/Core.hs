-- |
-- This module provides a low-level effectful API dealing with the connections to the database.
module Hasql.Connection.Core where

import Hasql.Connection.Config qualified as Config
import Hasql.Connection.Setting qualified as Setting
import Hasql.IO qualified as IO
import Hasql.LibPq14 qualified as LibPQ
import Hasql.Prelude
import Hasql.PostgresTypeInfo qualified as PTI
import Hasql.PreparedStatementRegistry qualified as PreparedStatementRegistry
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)

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
      -- | OID cache for custom types by name.
      !(MVar (Map Text PTI.OID))

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
    oidCache <- lift (newMVar mempty)
    pure (Connection (Config.usePreparedStatements config) pqConnectionRef integerDatetimes registry oidCache)
  where
    config = Config.fromUpdates settings

-- |
-- Release the connection.
release :: Connection -> IO ()
release (Connection _ pqConnectionRef _ _ _) =
  mask_ $ do
    nullConnection <- LibPQ.newNullConnection
    pqConnection <- swapMVar pqConnectionRef nullConnection
    IO.releaseConnection pqConnection

-- |
-- Execute an operation on the raw @libpq@ 'LibPQ.Connection'.
--
-- The access to the connection is exclusive.
withLibPQConnection :: Connection -> (LibPQ.Connection -> IO a) -> IO a
withLibPQConnection (Connection _ pqConnectionRef _ _ _) =
  withMVar pqConnectionRef

-- |
-- Look up an OID in the cache by type name.
lookupOidInCache :: Connection -> Text -> IO (Maybe PTI.OID)
lookupOidInCache (Connection _ _ _ _ oidCacheRef) typeName =
  readMVar oidCacheRef >>= pure . Map.lookup typeName

-- |
-- Add an OID to the cache.
addOidToCache :: Connection -> Text -> PTI.OID -> IO ()
addOidToCache (Connection _ _ _ _ oidCacheRef) typeName oid =
  modifyMVar_ oidCacheRef (pure . Map.insert typeName oid)
