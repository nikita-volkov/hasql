module HighSQL.Pool where

import HighSQL.Prelude hiding (read, Read, write, Write, Error)
import qualified Data.Pool as Pool
import qualified HighSQL.Backend as Backend
import qualified HighSQL.RowParser as RowParser
import qualified ListT


-- |
-- A pool of connections to the database.
type Pool b = 
  Pool.Pool (Backend.Connection b)

-- |
-- Pool initization settings.
data Settings =
  Settings {
    -- | 
    -- The number of stripes (distinct sub-pools) to maintain. 
    -- The smallest acceptable value is 1.
    striping1 :: Word32,
    -- |
    -- The maximum number of connections to keep open per a pool stripe. 
    -- The smallest acceptable value is 1.
    -- Requests for connections will block if this limit is reached 
    -- on a single stripe, 
    -- even if other stripes have idle connections available.
    striping2 :: Word32,
    -- |
    -- The amount of time for which an unused connection is kept open. 
    -- The smallest acceptable value is 0.5 seconds.
    connectionTimeout :: NominalDiffTime
  }

-- |
-- Initialize a pool given a backend and settings 
-- and run an IO computation with it, 
-- while automating the resource management.
withPool :: Backend.Backend b => b -> Settings -> (Pool b -> IO a) -> IO a
withPool b s =
  bracket acquire Pool.purgePool
  where
    acquire = 
      do
        pool <-
          Pool.createPool 
            (Backend.connect b) (Backend.disconnect) 
            (striping1 s) (connectionTimeout s) (striping2 s)
        return pool

withConnection :: (Backend.Connection b -> IO r) -> Pool b -> IO r
withConnection = flip Pool.withResource
