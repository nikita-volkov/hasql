{-# LANGUAGE UndecidableInstances #-}
-- |
-- This is the API of the \"hasql\" library.
-- For an introduction to the package 
-- and links to more documentation please refer to 
-- <../ the package's index page>.
-- 
-- This API is completely disinfected from exceptions. 
-- All error-reporting is explicit and 
-- is presented using the 'Either' type.
module Hasql
(
  -- * Pool
  Pool,
  acquirePool,
  releasePool,

  -- ** Pool Settings
  PoolSettings,
  poolSettings,

  -- * Session
  Session,
  session,

  -- ** Session Error
  SessionError(..),

  -- * Statement
  Bknd.Stmt,
  stmt,

  -- ** Statement Execution
  unitTx,
  countTx,
  singleTx,
  maybeTx,
  listTx,
  vectorTx,
  streamTx,

  -- * Transaction
  Tx,
  tx,

  -- ** Transaction Settings
  Bknd.TxMode(..),
  Bknd.TxIsolationLevel(..),
  Bknd.TxWriteMode(..),

  -- ** Result Stream
  TxListT,

  -- * Row Parser
  CxRow.CxRow,
)
where

import Hasql.Prelude
import qualified Hasql.Backend as Bknd
import qualified Hasql.CxRow as CxRow
import qualified Hasql.QParser as QParser
import qualified ListT
import qualified Data.Pool as Pool
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Hasql.TH as THUtil


-- * Resources
-------------------------

-- |
-- A connection pool. 
newtype Pool c =
  Pool (Pool.Pool (Either (Bknd.CxError c) c))

-- |
-- Given backend-specific connection settings and pool settings, 
-- acquire a backend connection pool,
-- which can then be used to work with the DB.
-- 
-- When combining Hasql with other libraries, 
-- which throw exceptions it makes sence to utilize 
-- @Control.Exception.'bracket'@
-- like this:
-- 
-- >bracket (acquirePool bkndStngs poolStngs) (releasePool) $ \pool -> do
-- >  session pool $ do
-- >    ...
-- >  ... any other IO code
acquirePool :: Bknd.Cx c => Bknd.CxSettings c -> PoolSettings -> IO (Pool c)
acquirePool cxSettings (PoolSettings size timeout) =
  fmap Pool $
  Pool.createPool (Bknd.acquireCx cxSettings) 
                  (either (const $ return ()) Bknd.releaseCx) 
                  (1)
                  (fromIntegral timeout) 
                  (size)

-- |
-- Release all connections acquired by the pool.
releasePool :: Pool c -> IO ()
releasePool (Pool p) =
  Pool.destroyAllResources p


-- ** Pool Settings
-------------------------

-- |
-- Settings of a pool.
data PoolSettings =
  PoolSettings !Int !Int
  deriving (Show)

instance TH.Lift PoolSettings where
  lift (PoolSettings a b) = 
    [|PoolSettings a b|]
    

-- | 
-- A smart constructor for pool settings.
poolSettings :: 
  Int
  -- ^
  -- The maximum number of connections to keep open. 
  -- The smallest acceptable value is 1.
  -- Requests for connections will block if this limit is reached.
  -> 
  Int
  -- ^
  -- The amount of seconds for which an unused connection is kept open. 
  -- The smallest acceptable value is 1.
  -> 
  Maybe PoolSettings
  -- ^
  -- Maybe pool settings, if they are correct.
poolSettings size timeout =
  if size > 0 && timeout >= 1
    then Just $ PoolSettings size timeout
    else Nothing


-- * Session
-------------------------

-- |
-- A convenience wrapper around 'ReaderT', 
-- which provides a shared context for execution and error-handling of transactions.
newtype Session c m r =
  Session (ReaderT (Pool c) (EitherT (SessionError c) m) r)
  deriving (Functor, Applicative, Monad, MonadIO, MonadError (SessionError c))

instance MonadTrans (Session c) where
  lift = Session . lift . lift

instance MonadTransControl (Session c) where
  type StT (Session c) a = Either (SessionError c) a
  liftWith onUnlift =
    Session $ ReaderT $ \e -> 
      lift $ onUnlift $ \(Session m) -> 
        runEitherT $ flip runReaderT e $ m
  restoreT = 
    Session . ReaderT . const . EitherT

deriving instance MonadBase IO m => MonadBase IO (Session c m)

instance (MonadBaseControl IO m) => MonadBaseControl IO (Session c m) where
  type StM (Session c m) a = ComposeSt (Session c) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance MFunctor (Session c) where
  hoist f (Session m) = 
    Session $ ReaderT $ \e ->
      EitherT $ f $ runEitherT $ flip runReaderT e $ m

-- |
-- Execute a session using an established connection pool.
-- 
-- This is merely a wrapper around 'runReaderT',
-- so you can run it around every transaction,
-- if you want.
session :: Pool c -> Session c m a -> m (Either (SessionError c) a)
session pool m =
  runEitherT $ flip runReaderT pool $ case m of Session m -> m


-- * Transaction
-------------------------

-- |
-- A transaction specialized for a backend connection @c@, 
-- associated with its intermediate results using an anonymous type-argument @s@ (same trick as in 'ST')
-- and producing a result @r@.
-- 
-- Running `IO` in `Tx` is prohibited. 
-- The motivation is identical to `STM`: 
-- the `Tx` block may get executed multiple times if any transaction conflicts arise. 
-- This will result in your effectful `IO` code being executed 
-- an unpredictable amount of times as well, 
-- which, chances are, is not what you want.
newtype Tx c s r =
  Tx { unwrapTx :: EitherT (SessionError c) (Bknd.Tx c) r }
  deriving (Functor, Applicative, Monad)


data SessionError c =
  -- |
  -- A backend-specific connection acquisition error.
  -- E.g., a failure to establish a connection.
  CxError (Bknd.CxError c) |
  -- |
  -- A backend-specific transaction error.
  -- It should cover all possible failures related to an established connection,
  -- including the loss of connection, query errors and database failures.
  TxError (Bknd.TxError c) |
  -- |
  -- Attempt to parse a result into an incompatible type.
  -- Indicates either a mismatching schema or an incorrect query.
  ResultError Text

deriving instance (Show (Bknd.CxError c), Show (Bknd.TxError c)) => Show (SessionError c)
deriving instance (Eq (Bknd.CxError c), Eq (Bknd.TxError c)) => Eq (SessionError c)

-- |
-- Execute a transaction in a session.
-- 
-- This function ensures on the type level, 
-- that it's impossible to return @'TxListT' s m r@ from it.
tx :: (Bknd.CxTx c, MonadBaseControl IO m) => Bknd.TxMode -> (forall s. Tx c s r) -> Session c m r
tx mode (Tx m) =
  Session $ ReaderT $ \(Pool pool) ->
    Pool.withResource pool $ \e -> do
      c <- hoistEither $ mapLeft CxError e
      let
        attempt =
          do
            r <- EitherT $ liftBase $ fmap (either (Left . TxError) Right) $ 
                 Bknd.runTx c mode $ runEitherT m
            maybe attempt hoistEither r
        in attempt


-- * Statements execution
-------------------------

-- |
-- Execute a statement without processing the result.
unitTx :: Bknd.Stmt c -> Tx c s ()
unitTx = 
  Tx . lift . Bknd.unitTx

-- |
-- Execute a statement and count the amount of affected rows.
-- Useful for resolving how many rows were updated or deleted.
countTx :: Bknd.CxValue c Word64 => Bknd.Stmt c -> Tx c s Word64
countTx =
  Tx . lift . Bknd.countTx

-- |
-- Execute a statement,
-- which produces exactly one result row.
-- E.g., @INSERT@, which returns an autoincremented identifier,
-- or @SELECT COUNT@, or @SELECT EXISTS@.
-- 
-- Please note that using this executor for selecting rows is conceptually wrong, 
-- since in that case the results are always optional. 
-- Use 'maybeTx', 'listTx' or 'vectorTx' instead.
-- 
-- If the result is empty this executor will raise 'ResultError'.
singleTx :: CxRow.CxRow c r => Bknd.Stmt c -> Tx c s r
singleTx =
  join . fmap (maybe (Tx $ left $ ResultError "No rows on 'singleTx'") return) .
  maybeTx

-- |
-- Execute a statement,
-- which optionally produces a single result row.
maybeTx :: CxRow.CxRow c r => Bknd.Stmt c -> Tx c s (Maybe r)
maybeTx =
  fmap (fmap Vector.unsafeHead . mfilter (not . Vector.null) . Just) . vectorTx

-- |
-- Execute a statement,
-- and produce a list of results.
listTx :: CxRow.CxRow c r => Bknd.Stmt c -> Tx c s [r]
listTx =
  fmap toList . vectorTx

-- |
-- Execute a statement,
-- and produce a vector of results.
vectorTx :: CxRow.CxRow c r => Bknd.Stmt c -> Tx c s (Vector r)
vectorTx s =
  Tx $ do
    r <- lift $ Bknd.vectorTx s
    EitherT $ return $ traverse ((mapLeft ResultError) . CxRow.parseRow) $ r

-- |
-- Given a batch size, execute a statement with a cursor,
-- and produce a result stream.
-- 
-- The cursor allows you to fetch virtually limitless results in a constant memory
-- at a cost of a small overhead.
-- 
-- The batch size parameter controls how many rows will be fetched 
-- during every roundtrip to the database. 
-- A minimum value of 256 seems to be sane.
-- 
-- Note that in most databases cursors require establishing a database transaction,
-- so depending on a backend the transaction may result in an error,
-- if you run it improperly.
streamTx :: CxRow.CxRow c r => Int -> Bknd.Stmt c -> Tx c s (TxListT s (Tx c s) r)
streamTx n s =
  Tx $ do
    r <- lift $ Bknd.streamTx n s
    return $ TxListT $ do
      row <- hoist (Tx . lift) r
      lift $ Tx $ EitherT $ return $ mapLeft ResultError $ CxRow.parseRow $ row


-- * Result Stream
-------------------------

-- |
-- A stream of results, 
-- which fetches only those that you reach.
-- 
-- It's a wrapper around 'ListT.ListT', 
-- which uses the same trick as the 'ST' monad to associate with the
-- context transaction and become impossible to be used outside of it.
-- This lets the library ensure that it is safe to automatically
-- release all the connections associated with this stream.
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


-- * Statements quasi-quotation
-------------------------

-- |
-- Produces a lambda-expression, 
-- which takes as many parameters as there are placeholders in the quoted text
-- and results in a 'Bknd.Stmt'. 
-- 
-- E.g.:
-- 
-- >selectSum :: Int -> Int -> Stmt c
-- >selectSum = [stmt|SELECT (? + ?)|]
-- 
stmt :: TH.QuasiQuoter
stmt = 
  TH.QuasiQuoter
    (parseExp)
    (const $ fail "Pattern context is not supported")
    (const $ fail "Type context is not supported")
    (const $ fail "Declaration context is not supported")
  where
    parseExp s =
      do
        (t, n) <- either (fail . showString "Parsing failure: ") return (QParser.parse (fromString s))
        return $ statementF (Text.unpack t) (fromIntegral n)
    statementF s n =
      TH.LamE
        (map TH.VarP argNames)
        (THUtil.purify [|Bknd.Stmt $(pure statementE) $(pure argsE) True|])
      where
        argNames = 
          map (TH.mkName . ('_' :) . show) [1 .. n]
        statementE = 
          TH.LitE (TH.StringL s)
        argsE =
          THUtil.vectorE $
          map (\x -> THUtil.purify [| Bknd.encodeValue $(TH.varE x) |]) $
          argNames
