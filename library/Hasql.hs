module Hasql
(
  -- * Session
  Session,
  session,

  -- ** Session Settings
  SessionSettings,
  sessionSettings,

  -- * Error
  Error(..),

  -- * Transaction
  Tx,
  Mode,
  Backend.IsolationLevel(..),
  tx,

  -- * Statement Quasi-Quoter
  QQ.q,

  -- * Statement Execution
  unit,
  count,
  single,
  stream,

  -- * Results Stream
  TxListT,

  -- * Row parser
  RowParser.RowParser(..),
)
where

import Hasql.Prelude hiding (Error)
import Hasql.Backend (Backend)
import Hasql.RowParser (RowParser)
import qualified Hasql.Backend as Backend
import qualified Hasql.RowParser as RowParser
import qualified Hasql.QQ as QQ
import qualified ListT
import qualified Data.Pool as Pool


-- * Session
-------------------------

-- |
-- A monad transformer,
-- which executes transactions.
type Session b =
  ReaderT (Pool b)

-- |
-- Given backend settings, session settings, and a session monad transformer,
-- execute it in the inner monad.
session :: 
  Backend.Backend b => MonadBaseControl IO m =>
  b -> SessionSettings -> Session b m r -> m r
session backend settings reader =
  join $ liftM restoreM $ liftBaseWith $ \runInIO ->
    mask $ \unmask -> do
      p <- acquirePool backend settings
      r <- ($ releasePool p) $ onException $ unmask $ runInIO $ runReaderT reader p
      releasePool p
      return r


-- ** Session Settings
-------------------------

-- |
-- Settings of a session.
data SessionSettings =
  SessionSettings !Word32 !NominalDiffTime

-- | 
-- A smart constructor for session settings.
sessionSettings :: 
  Word32
  -- ^
  -- The maximum number of connections to keep open. 
  -- The smallest acceptable value is 1.
  -- Requests for connections will block if this limit is reached.
  -> 
  NominalDiffTime
  -- ^
  -- The amount of time for which an unused connection is kept open. 
  -- The smallest acceptable value is 0.5 seconds.
  -> 
  Maybe SessionSettings
  -- ^
  -- Maybe session settings, if they are correct.
sessionSettings size timeout =
  if size > 0 && timeout >= 0.5
    then Just $ SessionSettings size timeout
    else Nothing


-- * Connections Pool
-------------------------

-- |
-- A connections pool.
newtype Pool b =
  Pool (Pool.Pool (Backend.Connection b))

-- |
-- Initialize a pool given a backend and settings.
acquirePool :: Backend.Backend b => b -> SessionSettings -> IO (Pool b)
acquirePool b (SessionSettings size timeout) =
  fmap Pool $
    Pool.createPool (Backend.connect b) (Backend.disconnect) 1 timeout size

-- |
-- Release all resources of the pool.
releasePool :: Pool b -> IO ()
releasePool (Pool p) = 
  Pool.purgePool p

usePool :: (Backend.Connection b -> IO a) -> Pool b -> IO a
usePool f (Pool p) =
  Pool.withResource p f


-- * Transaction
-------------------------

-- |
-- A transaction specialized for backend @b@, 
-- running on an anonymous state-thread @s@ 
-- and producing a result @r@.
newtype Tx b s r =
  Tx (ReaderT (Backend.Connection b) IO r)
  deriving (Functor, Applicative, Monad)

-- |
-- A transaction mode defining how a transaction should be executed.
-- 
-- * @Just (isolationLevel, write)@ indicates that a database transaction
-- should be established with a specified isolation level and a boolean, 
-- defining, whether it would perform any modification operations.
-- 
-- * @Nothing@ indicates that there should be no database transaction established on
-- the backend and therefore it should be executed with no ACID guarantees,
-- but also without any induced overhead.
type Mode =
  Maybe (Backend.IsolationLevel, Bool)

-- |
-- Execute a transaction in a session.
tx :: 
  Backend.Backend b => MonadBase IO m =>
  Mode -> (forall s. Tx b s r) -> Session b m r
tx m t =
  ReaderT $ \p -> liftBase $ usePool (\c -> runTx c m t) p
  where
    runTx ::
      Backend b => 
      Backend.Connection b -> Mode -> (forall s. Tx b s r) -> IO r
    runTx connection mode (Tx reader) =
      handle backendHandler $ 
        maybe (const id) inTransaction mode connection (runReaderT reader connection)
      where
        inTransaction ::
          Backend b => 
          Backend.TransactionMode -> Backend.Connection b -> IO r -> IO r
        inTransaction mode c io =
          do
            Backend.beginTransaction mode c
            try io >>= \case
              Left Backend.TransactionConflict -> do
                Backend.finishTransaction False c
                inTransaction mode c io
              Left e -> throwIO e
              Right r -> do
                Backend.finishTransaction True c
                return r
        backendHandler :: Backend.Error -> IO a
        backendHandler =
          \case
            Backend.CantConnect t -> throwIO $ CantConnect t
            Backend.ConnectionLost t -> throwIO $ ConnectionLost t
            Backend.UnexpectedResultStructure t -> throwIO $ UnexpectedResultStructure t
            Backend.TransactionConflict -> $bug "Unexpected TransactionConflict exception"


-- * Results Stream
-------------------------

-- |
-- A stream of results, 
-- which fetches only those that you reach.
-- 
-- It's a wrapper around 'ListT.ListT', 
-- which uses the same trick as the 'ST' monad to associate with the
-- context transaction and become impossible to be used outside of it.
-- This lets the library ensure that it is safe to automatically
-- release all the resources associated with this stream.
-- 
-- All the functions of the \"list-t\" library are applicable to this type,
-- amongst which are 'ListT.head', 'ListT.toList', 'ListT.fold', 'ListT.traverse_'.
newtype TxListT s m r =
  TxListT (ListT.ListT m r)
  deriving (Functor, Applicative, Alternative, Monad, MonadTrans, MonadPlus, 
            Monoid, ListT.ListMonad)

instance ListT.ListTrans (TxListT s) where
  uncons = 
    unsafeCoerce 
      (ListT.uncons :: ListT.ListT m r -> m (Maybe (r, ListT.ListT m r)))


-- * Error
-------------------------

-- |
-- The only exception type that this API can raise.
data Error =
  -- |
  -- Cannot connect to a server.
  CantConnect Text |
  -- |
  -- The connection got interrupted.
  ConnectionLost Text |
  -- |
  -- Unexpected result structure.
  -- Indicates usage of inappropriate statement executor.
  UnexpectedResultStructure Text |
  -- |
  -- Attempt to parse a statement execution result into an incompatible type.
  -- Indicates either a mismatching schema or an incorrect query.
  ResultParsingError Text
  deriving (Show, Typeable)

instance Exception Error


-- * Statements execution
-------------------------

-- |
-- Execute a statement, which produces no result.
unit :: Backend b => Backend.Statement b -> Tx b s ()
unit s =
  Tx $ ReaderT $ Backend.execute s

-- |
-- Execute a statement and count the amount of affected rows.
-- Useful for resolving how many rows were updated or deleted.
count :: Backend b => Backend.Mapping b Word64 => Backend.Statement b -> Tx b s Word64
count s =
  Tx $ ReaderT $ Backend.executeAndCountEffects s

-- |
-- Execute a statement,
-- which produces a single result row: 
-- a @SELECT@ 
-- or an @INSERT@, which produces a generated value (e.g., an auto-incremented id).
single :: Backend b => RowParser b r => Backend.Statement b -> Tx b s (Maybe r)
single s =
  ListT.head $ stream False s

-- |
-- Execute a @SELECT@ statement,
-- and produce a results stream.
-- The boolean parameter specifies, 
-- whether to utilize a cursor.
-- 
-- Cursor allows you to fetch virtually limitless results in a constant memory
-- at a cost of a small overhead.
-- Note that in most databases cursors require establishing a database transaction.
stream :: Backend b => RowParser b r => Bool -> Backend.Statement b -> TxListT s (Tx b s) r
stream cursor s =
  do
    r <- lift $ Tx $ ReaderT $ \c -> executor s c
    hoistBackendStream r
  where
    executor = 
      if cursor then Backend.executeAndStreamWithCursor else Backend.executeAndStream
    hoistBackendStream (w, s) =
      TxListT $ hoist (Tx . lift) $ do
        row <- ($ s) $ ListT.slice $ fromMaybe ($bug "Invalid row width") $ ListT.positive w
        either (lift . throwIO . ResultParsingError) return $ RowParser.parseRow row
