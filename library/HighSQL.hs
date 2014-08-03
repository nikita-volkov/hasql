module HighSQL where

import HighSQL.Prelude hiding (read, Read, write, Write)
import qualified HighSQL.Backend as Backend
import qualified Data.Pool as Pool


-- | 
-- A session, in which transactions are executed.
-- It maintains the shared state between transactions.
type S =
  ReaderT (Pool.Pool Backend.Connection)

-- |
-- Run the session monad transformer in the base monad.
session :: Backend.Backend -> S m r -> m r
session b s =
  $notImplemented
