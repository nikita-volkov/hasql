module Hasql.Transaction where

import Hasql.Prelude hiding (Read, Write, Error)
import Hasql.Backend (Backend)
import Hasql.RowParser (RowParser)
import qualified Hasql.Backend as Backend
import qualified Hasql.RowParser as RowParser
import qualified ListT


-- |
-- A transaction specialized for backend @b@, 
-- running on an anonymous state-thread @s@ 
-- and producing a result @r@.
newtype Transaction b s r =
  Transaction (ReaderT (Backend.Connection b) IO r)
  deriving (Functor, Applicative, Monad)

type Mode =
  Maybe (Backend.IsolationLevel, Bool)

txIO ::
  Backend b => 
  Mode -> Backend.Connection b -> (forall s. Transaction b s r) -> IO r
txIO mode connection (Transaction reader) =
  handle backendHandler $ 
    maybe (const id) (inTransaction) mode
      connection 
      (runReaderT reader connection)

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
-- It is implemented as a wrapper around 'ListT.ListT',
-- hence all the utility functions of the list transformer API 
-- are applicable to this type.
-- 
-- It uses the same trick as 'ST' to become impossible to be 
-- executed outside of its transaction.
-- Therefore you can only access it while remaining in a transaction,
-- and, when the transaction finishes,
-- all the acquired resources get automatically released.
type ResultsStream b s r =
  TransactionListT s (Transaction b s) r

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


-- * Transactions
-------------------------

type StatementTx b s r =
  Backend b =>
  Backend.Statement b -> Transaction b s r

-- |
-- Execute a statement, which produces no result.
unitTx :: StatementTx b s ()
unitTx s =
  Transaction $ ReaderT $ Backend.execute s

-- |
-- Execute a statement and count the amount of affected rows.
-- Useful for resolving how many rows were updated or deleted.
countTx :: (Backend.Mapping b Integer) => StatementTx b s Integer
countTx s =
  Transaction $ ReaderT $ Backend.executeAndCountEffects s

-- |
-- Execute a statement,
-- which produces a results stream: 
-- a @SELECT@ or an @INSERT@, 
-- which produces a generated value (e.g., an auto-incremented id).
streamTx :: RowParser b r => StatementTx b s (ResultsStream b s r)
streamTx s =
  Transaction $ ReaderT $ \c -> do
    fmap hoistBackendStream $ Backend.executeAndStream s c

-- |
-- Execute a @SELECT@ statement
-- and produce a results stream, 
-- which utilizes a database cursor.
-- This function allows you to fetch virtually limitless results in a constant memory.
cursorStreamTx :: (RowParser b r) => StatementTx b s (ResultsStream b s r)
cursorStreamTx s =
  Transaction $ ReaderT $ \c -> do
    fmap hoistBackendStream $ Backend.executeAndStreamWithCursor s c

    
-- * Helpers
-------------------------

hoistBackendStream :: RowParser b r => Backend.ResultsStream b -> ResultsStream b s r
hoistBackendStream (w, s) =
  TransactionListT $ hoist (Transaction . lift) $ do
    row <- ($ s) $ ListT.slice $ fromMaybe ($bug "Invalid row width") $ ListT.positive w
    either (lift . throwIO . ResultParsingError) return $ RowParser.parse row
      
