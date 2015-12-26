{-# OPTIONS_GHC -funbox-strict-fields #-}

module Hasql.Connection.Impl
where

import Hasql.Prelude
import Control.Concurrent.MVar (newMVar, swapMVar)
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Hasql.PreparedStatementRegistry as PreparedStatementRegistry
import qualified Hasql.IO as IO


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
acquire :: ByteString -> IO (Either ConnectionError Connection)
acquire settings =
  {-# SCC "acquire" #-}
  runEitherT $ do
    pqConnection <- lift (IO.acquireConnection settings)
    lift (IO.checkConnectionStatus pqConnection) >>= traverse left
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

{-# INLINE withConnectionRef #-}
withConnectionRef :: MVar LibPQ.Connection -> (LibPQ.Connection -> IO a) -> IO a
withConnectionRef =
  withMVar
