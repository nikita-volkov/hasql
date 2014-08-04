module HighSQL.Transaction where

import HighSQL.Prelude hiding (read, Read, write, Write)
import qualified Data.Pool as Pool
import qualified Database.HDBC as HDBC
import qualified HighSQL.CompositionT as CompositionT
import qualified HighSQL.Session as Session


-- |
-- A transaction with a level @l@ and a result @r@.
data T l s r

transaction :: MonadIO m => (forall s. T l s r) -> Session.S m r
transaction = $notImplemented


-- * Levels
-------------------------

data Read

instance SelectPrivilege Read


data Write

instance SelectPrivilege Write
instance UpdatePrivilege Write


data Admin

instance CreatePrivilege Admin


-- * Privileges
-------------------------

-- |
-- "select"
class SelectPrivilege l

select :: SelectPrivilege l => Statement -> ResultsStream s (T l s) r
select = 
  $notImplemented


-- |
-- "update", "insert", "delete"
class UpdatePrivilege l

update :: UpdatePrivilege l => Statement -> T l s ()
update =
  $notImplemented


-- |
-- "create", "alter", "drop", "truncate"
class CreatePrivilege l

create :: CreatePrivilege l => Statement -> T l s ()
create =
  $notImplemented


-- * Statements
-------------------------

data Statement =
  Statement !ByteString ![HDBC.SqlValue]
  deriving (Show, Eq)


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
