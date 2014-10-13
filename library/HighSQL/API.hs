module HighSQL.API where

import HighSQL.Prelude hiding (read, Read, write, Write, Error)
import qualified Data.Pool as Pool
import qualified HighSQL.Backend as Backend
import qualified HighSQL.RowParser as RowParser
import qualified ListT


type Backend =
  Backend.Backend

type RowParser =
  RowParser.RowParser


-- * Pool
-------------------------

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
withPool :: Backend b => b -> Settings -> (Pool b -> IO a) -> IO a
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


-- * Executors
-------------------------

-- |
-- A connections 'Pool' context monad.
type Executor b =
  ReaderT (Pool b) IO

executorIO :: Pool b -> Executor b r -> IO r
executorIO = 
  flip runReaderT

writeTransactionExecutor :: 
  Backend b =>
  Backend.IsolationLevel -> (forall s. Transaction b Write s r) -> Executor b r
writeTransactionExecutor isolation (Transaction t) = 
  withConnectionExecutor $ \c -> 
    Backend.inTransaction (isolation, True) (runReaderT t c) c

streamingExecutor :: 
  forall b r.
  Backend b => RowParser b r => Typeable r =>
  Backend.Statement b -> Executor b (ListT (Executor b) r)
streamingExecutor s =
  withConnectionExecutor $ \c -> do
    (w, s) <- Backend.executeAndStream s c
    return $ do
      row <- hoist lift $ replicateM w s
      maybe (lift $ lift $ throwIO parsingError) return $ RowParser.parse row
  where
    parsingError =
      ResultParsingError (fst s) (typeOf (undefined :: r))

countingExecutor :: 
  Backend b => 
  Backend.Statement b -> Executor b Integer
countingExecutor s =
  withConnectionExecutor $ \c -> do
    Backend.executeAndCountEffects s c

generatingExecutor :: 
  Backend b => Backend.Mapping b Integer => 
  Backend.Statement b -> Executor b (Maybe Integer)
generatingExecutor s =
  streamingExecutor s >>= ListT.head

unitExecutor ::
  Backend b => 
  Backend.Statement b -> Executor b ()
unitExecutor s =
  withConnectionExecutor $ \c -> do
    Backend.execute s c

withConnectionExecutor :: 
  (Backend.Connection b -> IO r) -> Executor b r
withConnectionExecutor f =
  ReaderT $ \pool -> Pool.withResource pool $ \c -> handle handler $ f c
  where
    handler =
      \case
        Backend.ConnectionLost m -> throwIO $ ConnectionLost m


-- * Transaction
-------------------------

-- |
-- A transaction with a level @l@,
-- running on an anonymous state-thread @s@ 
-- and gaining a result @r@.
newtype Transaction b l s r =
  Transaction (ReaderT (Backend.Connection b) IO r)
  deriving (Functor, Applicative, Monad)

streamingTransaction ::
  Backend b => RowParser b r => ReadingPrivilege l =>
  Backend.Statement b -> Transaction b l s (ListT (Transaction b l s) r)
streamingTransaction statement =
  Transaction $ ReaderT $ \connection ->
    $notImplemented

-- |
-- Perform a select, while utilizing the database cursors functionality,
-- which allows to query for virtually unlimited result sets 
-- in constant memory by utilizing streaming.
-- 
-- However using this function for small result sets isn't beneficial,
-- since it introduces a small overhead due to bookkeeping related to cursors.
-- 
-- All resources are automatically managed
-- and get released on transaction finish.
streamingWithCursorTransaction :: 
  Backend b => RowParser b r => ReadingPrivilege l =>
  Backend.Statement b -> Transaction b l s (TransactionListT s (Transaction b l s) r)
streamingWithCursorTransaction =
  undefined


-- * Results Stream
-------------------------

-- |
-- A stream of results, 
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


-- ** Levels
-------------------------

-- |
-- Requires minimal locking from the database,
-- however you can only execute the \"SELECT\" statements in it. 
data Read

-- |
-- Allows you to perform any kind of statements,
-- including \"SELECT\", \"UPDATE\", \"INSERT\", \"DELETE\",
-- \"CREATE\", \"DROP\" and \"ALTER\".
-- 
-- However, compared to 'Read', 
-- this transaction level requires the database to choose 
-- a more resource-demanding locking strategy.
data Write


-- ** Privileges
-------------------------

-- |
-- \"SELECT\"
class ReadingPrivilege l

instance ReadingPrivilege Read
instance ReadingPrivilege Write

-- |
-- \"UPDATE\", \"INSERT\", \"DELETE\",
-- \"CREATE\", \"ALTER\", \"DROP\", \"TRUNCATE\"
class ModificationPrivilege l

instance ModificationPrivilege Write


-- * Statement
-------------------------

-- type Backend.Statement b =
--   (ByteString, [Backend.StatementArgument b])

-- data Backend.Statement b =
--   Statement !ByteString ![Backend.StatementArgument b]

-- mkStatement :: forall b. ByteString -> [Value b] -> Backend.Statement b
-- mkStatement sql values =
--   (,) sql (map renderValue values)
--   where
--     renderValue (Value v) = 
--       Backend.renderValue v :: Backend.StatementArgument b

-- data Value b =
--   forall v. (Backend.Mapping b v) => Value !v


-- * Aliases
-------------------------

-- |
-- A short form alias to 'Executor'.
type E b = 
  Executor b

-- |
-- A short form alias to 'executorIO'.
runE :: Pool b -> E b r -> IO r
runE = executorIO

-- |
-- A short form alias to 'writeTransactionExecutor'.
writeE :: Backend b => Backend.IsolationLevel -> (forall s. T b W s r) -> E b r
writeE = writeTransactionExecutor

-- |
-- A short form alias to 'streamingExecutor'.
streamingE :: Typeable r => Backend b => RowParser b r => S b -> E b (ListT (E b) r)
streamingE = streamingExecutor

-- |
-- A short form alias to 'streamingTransaction'.
streamingT :: Backend b => RowParser b r => ReadingPrivilege l => S b -> T b l s (ListT (T b l s) r)
streamingT = streamingTransaction

-- |
-- A short form alias to 'Transaction'.
type T =
  Transaction

type W =
  Write

type R =
  Read

type TListT =
  TransactionListT

type S b =
  Backend.Statement b
