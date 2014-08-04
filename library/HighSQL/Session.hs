module HighSQL.Session where

import HighSQL.Prelude
import qualified Data.Pool as Pool
import qualified Database.HDBC as HDBC
import qualified HighSQL.Backend as Backend


-- | 
-- A session, in which transactions are executed.
-- It maintains the shared state between transactions.
type S =
  ReaderT (Pool.Pool HDBC.ConnWrapper)

-- |
-- Run the session monad transformer in the base monad.
session :: Backend.Backend -> S m r -> m r
session b s =
  $notImplemented


