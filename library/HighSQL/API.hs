module HighSQL.API where

import HighSQL.Prelude hiding (read, Read, write, Write)
import qualified Data.Pool as Pool
import qualified HighSQL.CompositionT as CompositionT
import qualified HighSQL.Backend as Backend
import qualified ListT


-- * Session
-------------------------

data Session = 
  Session !(Pool.Pool Backend.Connection)

-- |
-- Session settings.
data Settings =
  Settings {
    -- | 
    -- The number of stripes (distinct sub-pools) to maintain. 
    -- The smallest acceptable value is 1.
    striping1 :: Word32,
    -- |
    -- Maximum number of connections to keep open per stripe. 
    -- The smallest acceptable value is 1.
    -- Requests for connections will block if this limit is reached 
    -- on a single stripe, 
    -- even if other stripes have idle connections available.
    striping2 :: Word32,
    -- |
    -- The amount of time for which an unused resource is kept open. 
    -- The smallest acceptable value is 0.5 seconds.
    -- The elapsed time before destroying a resource 
    -- may be a little longer than requested, 
    -- as the reaper thread wakes at 1-second intervals.
    connectionTimeout :: NominalDiffTime
  }

withSession :: Backend.Backend -> Settings -> (Session -> IO a) -> IO a
withSession b s =
  bracket acquire release
  where
    acquire = 
      do
        pool <-
          Pool.createPool 
            (Backend.connect b) (Backend.disconnect) (striping1 s)
            (connectionTimeout s) (striping2 s)
        return (Session pool)
    release (Session pool) =
      Pool.purgePool pool


-- ** Error
-------------------------

data SessionError =
  TransactionError TransactionError
  deriving (Show, Typeable)


-- * Transaction
-------------------------

-- |
-- A transaction with a level @l@ and a result @r@.
newtype T l s r =
  T (CompositionT.T (ReaderT Backend.Connection (EitherT TransactionError IO)) r)
  deriving (Functor, Applicative, Monad)

transaction :: Session -> (forall s. T l s r) -> IO r
transaction (Session p) t = 
  do
    e <- 
      Pool.withResource p $ runEitherT . \c -> 
        case composed of
          False -> 
            runReaderT r c
          True ->
            do
              $notImplemented
    either throwIO return e
  where
    (composed, r) = case t of T t -> CompositionT.run t


-- ** Error
-------------------------

data TransactionError =
  TE
  deriving (Show, Typeable)

instance Exception TransactionError


-- * Levels
-------------------------

data Read

read :: MonadIO m => Session -> (forall s. T Read s r) -> IO r
read = transaction


data Write

write :: MonadIO m => Session -> (forall s. T Write s r) -> IO r
write = transaction


data Admin

admin :: MonadIO m => Session -> (forall s. T Admin s r) -> IO r
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

update :: UpdatePrivilege l => Statement -> T l s (Maybe Integer)
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
