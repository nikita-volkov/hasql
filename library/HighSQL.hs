module HighSQL where

import HighSQL.Prelude hiding (read, Read, write, Write)
import qualified HighSQL.Backend as Backend
import qualified HighSQL.CompositionT as CompositionT
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


-- * Transactions
-------------------------

-- |
-- A transaction with a level @l@ and a result @r@.
data T l r


-- ** Privileges
-------------------------

class SelectPrivilege l

select :: SelectPrivilege l => Select r -> T l r
select = 
  $notImplemented


class UpdatePrivilege l

update :: UpdatePrivilege l => Update -> T l ()
update =
  $notImplemented

-- class AlterPrivilege p where
--   alter :: Alter -> T p ()


-- ** Levels
-------------------------

data Read

instance SelectPrivilege Read


data Write

instance SelectPrivilege Write
instance UpdatePrivilege Write

data Admin


-- * Statements
-------------------------

newtype Select r =
  Select (Backend.Connection -> IO r)

newtype Update =
  Update (Backend.Connection -> IO ())

