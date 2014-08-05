module HighSQL.API where

import HighSQL.Prelude hiding (read, Read, write, Write)
import qualified Data.Pool as Pool
import qualified HighSQL.CompositionT as CompositionT
import qualified HighSQL.Backend as Backend
import qualified ListT


-- * Session
-------------------------

-- | 
-- A session, in which transactions are executed.
-- It maintains a shared state between transactions.
newtype S m r =
  S (ReaderT (Pool.Pool Backend.Connection) (EitherT SessionError m) r)
  deriving (Functor, Applicative, Monad, MonadIO)

-- |
-- Run the session monad transformer in the base monad.
session :: Backend.Backend -> S m r -> m r
session b s =
  $notImplemented


-- ** Error
-------------------------

data SessionError =
  TransactionError TransactionError


-- * Transaction
-------------------------

-- |
-- A transaction with a level @l@ and a result @r@.
newtype T l s r =
  T (ReaderT Backend.Connection (EitherT TransactionError IO) r)
  deriving (Functor, Applicative, Monad)

transaction :: MonadIO m => (forall s. T l s r) -> S m r
transaction t = 
  do
    pool <- S $ ask
    e <-
      liftIO $ Pool.withResource pool $ \c -> 
        runEitherT $ runReaderT (case t of T r -> r) c
    either (S . lift . left . TransactionError) return e


-- ** Error
-------------------------

data TransactionError


-- * Levels
-------------------------

data Read

read :: MonadIO m => (forall s. T Read s r) -> S m r
read = transaction


data Write

write :: MonadIO m => (forall s. T Write s r) -> S m r
write = transaction


data Admin

admin :: MonadIO m => (forall s. T Admin s r) -> S m r
admin = transaction


-- * Privileges
-------------------------

-- |
-- "SELECT"
class SelectPrivilege l

instance SelectPrivilege Read
instance SelectPrivilege Write

select :: SelectPrivilege l => Statement -> ResultsStream s (T l s) r
select = 
  $notImplemented


-- |
-- "UPDATE", "INSERT", "DELETE"
class UpdatePrivilege l

instance UpdatePrivilege Write

update :: UpdatePrivilege l => Statement -> T l s ()
update =
  $notImplemented


-- |
-- "CREATE", "ALTER", "DROP", "TRUNCATE"
class CreatePrivilege l

instance CreatePrivilege Admin

create :: CreatePrivilege l => Statement -> T l s ()
create =
  $notImplemented


-- * Statement
-------------------------

data Statement =
  Statement !ByteString ![Backend.Value]
  deriving (Show)


-- * Results Stream
-------------------------

-- |
-- A stream of results, 
-- which fetches only those that you reach.
-- 
-- Uses the same trick as 'ST' to become impossible to be run outside of
-- its transaction.
-- Hence you can only access it while remaining in transaction,
-- and when the transaction finishes it safely gets automatically released.
-- 
-- It is implemented as a wrapper around 'ListT.ListT',
-- hence all the utility functions of the list transformer API 
-- are applicable to this type.
newtype ResultsStream s m r =
  ResultsStream (ListT.ListT m r)
  deriving (Functor, Applicative, Alternative, Monad, MonadTrans, MonadPlus, 
            Monoid, ListT.ListMonad, ListT.ListTrans)
