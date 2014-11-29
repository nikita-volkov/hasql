-- |
-- This is the API of the \"hasql\" library.
-- For an introduction to the package 
-- and links to more documentation please refer to 
-- <../ the package's index page>.
module Hasql
(
  -- * Session
  Session,
  session,
  sessionUnlifter,

  -- ** Session Settings
  SessionSettings,
  sessionSettings,

  -- * Transaction
  Tx,
  tx,

  -- ** Transaction Settings
  Mode,
  Backend.IsolationLevel(..),

  -- * Statement Quasi-Quoter
  q,

  -- * Statement Execution
  unit,
  count,
  single,
  list,
  stream,

  -- * Results Stream
  TxListT,

  -- * Row parser
  RowParser.RowParser(..),
  
  -- * Error
  Error(..),
)
where

import Hasql.Prelude hiding (Error)
import Hasql.Backend (Backend)
import Hasql.RowParser (RowParser)
import qualified Hasql.Backend as Backend
import qualified Hasql.RowParser as RowParser
import qualified Hasql.QParser as QParser
import qualified ListT
import qualified Data.Pool as Pool
import qualified Data.Vector as Vector
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Hasql.TH as THUtil


-- * Session
-------------------------

-- |
-- A monad transformer,
-- which executes transactions.
-- 
-- * @b@ is a backend.
-- 
-- * @s@ is an anonymous variable, 
-- used to associate 'sessionUnlifter' with a specific session. 
-- 
-- * @m@ is an inner (transformed) monad.
-- 
-- * @r@ is a result.
newtype Session b s m r =
  Session (ReaderT (Pool.Pool (Backend.Connection b)) m r)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance MonadTransControl (Session b s) where
  newtype StT (Session b s) a = SessionStT a
  liftWith onRunner =
    Session $ ReaderT $ \e -> onRunner $ \(Session (ReaderT f)) -> liftM SessionStT $ f e
  restoreT = 
    Session . ReaderT . const . liftM (\(SessionStT a) -> a)

instance (MonadBase IO m) => MonadBase IO (Session b s m) where
  liftBase = Session . liftBase

instance (MonadBaseControl IO m) => MonadBaseControl IO (Session b s m) where
  newtype StM (Session b s m) a = SessionStM (ComposeSt (Session b s) m a)
  liftBaseWith = defaultLiftBaseWith SessionStM
  restoreM = defaultRestoreM $ \(SessionStM x) -> x


-- |
-- Given backend settings, session settings, and a session monad transformer,
-- execute it in the inner monad.
-- 
-- It uses the same trick as 'ST' with the anonymous @s@ type argument
-- to prohibit the use of the result of
-- 'sessionUnlifter' outside of its creator session.
session :: 
  (Backend.Backend b, MonadBaseControl IO m) =>
  b -> SessionSettings -> (forall s. Session b s m r) -> m r
session backend (SessionSettings size timeout) s =
  control $ \runInIO ->
    mask $ \unmask -> do
      p <- Pool.createPool (Backend.connect backend) Backend.disconnect 1 timeout size
      r <- try $ unmask $ runInIO $ runSession p s
      Pool.purgePool p
      either (throwIO :: SomeException -> IO r) return r

-- |
-- Get a session unlifting function, 
-- which allows to execute a session in the inner monad
-- using the resources of the current session.
-- 
-- Using this function in combination with 'lift'
-- you can interleave 'Session' with other monad transformers.
-- 
-- This function has the following property:
-- 
-- > (sessionUnlifter >>= \unlift -> lift (unlift m)) ≡ m
sessionUnlifter :: (MonadBaseControl IO m) => Session b s m (Session b s m r -> m r)
sessionUnlifter =
  Session $ ReaderT $ return . runSession

runSession :: (MonadBaseControl IO m) => Pool.Pool (Backend.Connection b) -> Session b s m r -> m r
runSession e (Session r) =
  control $ \runInIO -> 
    catch (runInIO (runReaderT r e)) $ \case
      Backend.CantConnect t -> throwIO $ CantConnect t
      Backend.ConnectionLost t -> throwIO $ ConnectionLost t
      Backend.ErroneousResult t -> throwIO $ ErroneousResult t
      Backend.UnexpectedResult t -> throwIO $ UnexpectedResult t
      Backend.UnparsableTemplate t -> throwIO $ UnparsableTemplate t
      Backend.TransactionConflict -> $bug "Unexpected TransactionConflict exception"
      Backend.NotInTransaction -> throwIO $ NotInTransaction


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


-- ** Error
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
  -- An error returned from the database.
  ErroneousResult Text |
  -- |
  -- Unexpected result structure.
  -- Indicates usage of inappropriate statement executor.
  UnexpectedResult Text |
  -- |
  -- Incorrect statement template.
  UnparsableTemplate Text |
  -- |
  -- An operation, 
  -- which requires a database transaction was executed without one.
  NotInTransaction |
  -- |
  -- Attempt to parse a row into an incompatible type.
  -- Indicates either a mismatching schema or an incorrect query.
  UnparsableRow Text
  deriving (Show, Typeable, Eq, Ord)

instance Exception Error


-- * Transaction
-------------------------

-- |
-- A transaction specialized for a backend @b@, 
-- associated with its intermediate results using an anonymous type-argument @s@ (same as in 'ST')
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
-- 
-- This function ensures on the type level, 
-- that it's impossible to return @'TxListT' s m r@ from it.
tx :: 
  (Backend.Backend b, MonadBase IO m) =>
  Mode -> (forall s. Tx b s r) -> Session b s m r
tx m t =
  Session $ ReaderT $ \p -> liftBase $ Pool.withResource p $ \c -> runTx c m t
  where
    runTx ::
      Backend b => 
      Backend.Connection b -> Mode -> (forall s. Tx b s r) -> IO r
    runTx connection mode (Tx reader) =
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
            Monoid, ListT.MonadCons)

instance ListT.MonadTransUncons (TxListT s) where
  uncons = 
    (liftM . fmap . fmap) (unsafeCoerce :: ListT.ListT m r -> TxListT s m r) .
    ListT.uncons . 
    (unsafeCoerce :: TxListT s m r -> ListT.ListT m r)


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
count :: (Backend b, Backend.Mapping b Word64) => Backend.Statement b -> Tx b s Word64
count s =
  Tx $ ReaderT $ Backend.executeAndCountEffects s

-- |
-- Execute a statement,
-- which produces a single result row: 
-- a @SELECT@ 
-- or an @INSERT@, which produces a generated value (e.g., an auto-incremented id).
single :: (Backend b, RowParser b r) => Backend.Statement b -> Tx b s (Maybe r)
single s =
  headMay <$> list s

-- |
-- Execute a @SELECT@ statement,
-- and produce a list of results.
list :: (Backend b, RowParser b r) => Backend.Statement b -> Tx b s [r]
list s =
  Tx $ ReaderT $ \c -> do
    m <- Backend.executeAndGetMatrix s c
    traverse (either (throwIO . UnparsableRow) return . RowParser.parseRow) $ Vector.toList m

-- |
-- Execute a @SELECT@ statement with a cursor,
-- and produce a results stream.
-- 
-- Cursor allows you to fetch virtually limitless results in a constant memory
-- at a cost of a small overhead.
-- Note that in most databases cursors require establishing a database transaction,
-- so a 'NotInTransaction' error will be raised if you run it improperly.
stream :: (Backend b, RowParser b r) => Backend.Statement b -> TxListT s (Tx b s) r
stream s =
  do
    s <- lift $ Tx $ ReaderT $ \c -> Backend.executeAndStream s c
    TxListT $ hoist (Tx . lift) $ do
      row <- s
      either (lift . throwIO . UnparsableRow) return $ RowParser.parseRow row


-- * Statements quasi-quotation
-------------------------

-- |
-- Produces a lambda-expression, 
-- which takes as many parameters as there are placeholders in the quoted text
-- and results in a 'Backend.Statement'. 
-- 
-- E.g.:
-- 
-- >selectFive :: Statement b
-- >selectFive = [q|SELECT (? + ?)|] 2 3
-- 
q :: TH.QuasiQuoter
q = 
  TH.QuasiQuoter
    (parseExp)
    (const $ fail "Pattern context is not supported")
    (const $ fail "Type context is not supported")
    (const $ fail "Declaration context is not supported")
  where
    parseExp s =
      do
        n <- either (fail . showString "Parsing failure: ") return (QParser.parse (fromString s))
        return $ statementF s n
    statementF s n =
      TH.LamE
        (map TH.VarP argNames)
        (THUtil.purify [|(,,) $(pure statementE) $(pure argsE) True|])
      where
        argNames = 
          map (TH.mkName . ('_' :) . show) [1 .. n]
        statementE = 
          TH.LitE (TH.StringL s)
        argsE = 
          TH.ListE $ flip map argNames $ \x ->
            THUtil.purify
            [| Backend.renderValue $(TH.varE x) |]
        
