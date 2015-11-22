module Hasql.Connection.Impl
where

import Hasql.Prelude
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Hasql.PreparedStatementRegistry as PreparedStatementRegistry
import qualified Hasql.IO as IO


-- |
-- A single connection to the database.
data Connection =
  Connection !LibPQ.Connection !Bool !PreparedStatementRegistry.PreparedStatementRegistry

-- |
-- Possible details of the connection acquistion error.
type ConnectionError =
  Maybe ByteString

-- |
-- Acquire a connection using the provided settings.
connect :: ByteString -> IO (Either ConnectionError Connection)
connect settings =
  {-# SCC "connect" #-} 
  runEitherT $ do
    pqConnection <- lift (IO.acquireConnection settings)
    lift (IO.checkConnectionStatus pqConnection) >>= traverse left
    lift (IO.initConnection pqConnection)
    integerDatetimes <- lift (IO.getIntegerDatetimes pqConnection)
    registry <- lift (IO.acquirePreparedStatementRegistry)
    pure (Connection pqConnection integerDatetimes registry)

-- |
-- Release the connection.
disconnect :: Connection -> IO ()
disconnect (Connection pqConnection _ _) =
  LibPQ.finish pqConnection
