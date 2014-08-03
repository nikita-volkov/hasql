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
data T l s r

transaction :: MonadIO m => (forall s. T l s r) -> S m r
transaction = $notImplemented


-- ** Privileges
-------------------------

class SelectPrivilege l

select :: SelectPrivilege l => Select r -> ResultsStream s (T l s) r
select = 
  $notImplemented


class UpdatePrivilege l

update :: UpdatePrivilege l => Update -> T l s ()
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


-- * Results Stream
-------------------------

-- |
-- A stream of results, 
-- which fetches only those that you reach.
-- 
-- Uses the same trick as 'ST' to become impossible to run outside of
-- its transaction.
-- Hence you can only access it while remaining in transaction,
-- and when the transaction finishes it safely gets automatically released.
data ResultsStream s (m :: * -> *) r
