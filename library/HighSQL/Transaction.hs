module HighSQL.Transaction where

import HighSQL.Prelude hiding (Read, Write, Error)
import qualified HighSQL.Backend as Backend
import qualified HighSQL.RowParser as RowParser
import qualified ListT


-- |
-- A transaction with a level @l@,
-- running on an anonymous state-thread @s@ 
-- and gaining a result @r@.
newtype Transaction b l s r =
  Transaction (ReaderT (Backend.Connection b) IO r)
  deriving (Functor, Applicative, Monad)

  
class Level l where
  run :: Backend b => Backend.Connection b -> l -> (forall s. Transaction b l s r) -> IO r

instance Level NoLocking where
  run c NoLocking (Transaction r) =
    handle backendHandler $ runReaderT r c

instance Level Read where
  run c (Read isolation) (Transaction r) =
    handle backendHandler $ Backend.inTransaction (isolation, False) (runReaderT r c) c

instance Level Write where
  run c (Write isolation) (Transaction r) =
    handle backendHandler $ Backend.inTransaction (isolation, True) (runReaderT r c) c


backendHandler :: Backend.Error -> IO a
backendHandler =
  \case
    Backend.ConnectionLost t -> throwIO $ ConnectionLost t


-- * Locking Levels
-------------------------

-- |
-- A level requiring no locking by the transaction
-- and hence providing no ACID guarantees.
-- Essentially this means that there will be no 
-- traditional transaction established on the backend.
data NoLocking = 
  NoLocking

-- |
-- A level requiring minimal locking from the database,
-- however it only allows to execute the \"SELECT\" statements. 
data Read =
  Read (Backend.IsolationLevel)

-- |
-- A level, which allows to perform any kind of statements,
-- including \"SELECT\", \"UPDATE\", \"INSERT\", \"DELETE\",
-- \"CREATE\", \"DROP\" and \"ALTER\".
-- 
-- However, compared to 'Read', 
-- it requires the database to choose 
-- a more resource-demanding locking strategy.
data Write =
  Write (Backend.IsolationLevel)


-- * Privileges
-------------------------

class CursorsPrivilege l

instance CursorsPrivilege Read
instance CursorsPrivilege Write

class ModificationPrivilege l

instance ModificationPrivilege Write
instance ModificationPrivilege NoLocking


-- * Results Stream
-------------------------

type ResultsStream b l s r =
  TransactionListT s (Transaction b l s) r

-- |
-- A select of results, 
-- which fetches only those that you reach.
-- 
-- It is implemented as a wrapper around 'ListT.ListT',
-- hence all the utility functions of the list transformer API 
-- are applicable to this type.
-- 
-- It uses the same trick as 'ST' to become impossible to be 
-- executed outside of its transaction.
-- Hence you can only access it while remaining in a transaction,
-- and, when the transaction finishes,
-- all the acquired resources get automatically released.
newtype TransactionListT s m r =
  TransactionListT (ListT.ListT m r)
  deriving (Functor, Applicative, Alternative, Monad, MonadTrans, MonadPlus, 
            Monoid, ListT.ListMonad)

instance ListT.ListTrans (TransactionListT s) where
  uncons = 
    unsafeCoerce 
      (ListT.uncons :: ListT.ListT m r -> m (Maybe (r, ListT.ListT m r)))


-- * Error
-------------------------

-- |
-- The only exception type that this API can raise.
data Error =
  -- |
  -- Cannot connect to a server 
  -- or the connection got interrupted.
  ConnectionLost Text |
  -- |
  -- Attempt to parse a statement execution result into an incompatible type.
  -- Indicates either a mismatching schema or an incorrect query.
  ResultParsingError ByteString TypeRep
  deriving (Show, Typeable)

instance Exception Error


-- * Transactions
-------------------------

type Backend =
  Backend.Backend

type RowParser =
  RowParser.RowParser

-- |
-- Execute a modification statement producing no result.
modify :: 
  Backend b => ModificationPrivilege l =>
  Backend.Statement b -> Transaction b l s ()
modify s =
  Transaction $ ReaderT $ Backend.execute s

-- |
-- Execute a statement, which generates an auto-increment value.
modifyAndGenerate :: 
  Backend b => Backend.Mapping b Integer => ModificationPrivilege l =>
  Backend.Statement b -> Transaction b l s (Maybe Integer)
modifyAndGenerate s =
  select s >>= ListT.head

modifyAndCount ::
  Backend b => Backend.Mapping b Integer => ModificationPrivilege l =>
  Backend.Statement b -> Transaction b l s Integer
modifyAndCount s =
  Transaction $ ReaderT $ Backend.executeAndCountEffects s

select :: 
  forall b l s r.
  Backend b => RowParser b r => Typeable r =>
  Backend.Statement b -> Transaction b l s (ResultsStream b l s r)
select s =
  Transaction $ ReaderT $ \c -> do
    (w, s) <- Backend.executeAndStream s c
    return $ TransactionListT $ hoist (Transaction . lift) $ do
      row <- replicateM w s
      maybe (lift $ throwIO parsingError) return $ RowParser.parse row
  where
    parsingError =
      ResultParsingError (fst s) (typeOf (undefined :: r))

selectWithCursor :: 
  forall b l s r.
  Backend b => RowParser b r => Typeable r => CursorsPrivilege l =>
  Backend.Statement b -> Transaction b l s (ResultsStream b l s r)
selectWithCursor s =
  Transaction $ ReaderT $ \c -> do
    (w, s) <- Backend.executeAndStreamWithCursor s c
    return $ TransactionListT $ hoist (Transaction . lift) $ do
      row <- replicateM w s
      maybe (lift $ throwIO parsingError) return $ RowParser.parse row
  where
    parsingError =
      ResultParsingError (fst s) (typeOf (undefined :: r))
