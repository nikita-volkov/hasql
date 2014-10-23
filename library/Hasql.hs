module Hasql
(
  -- * Connections Pool
  Pool.Pool,
  Pool.Settings(..),
  Pool.withPool,

  -- * Transaction
  Transaction.Transaction,
  Transaction.Mode,

  -- ** Execution
  -- | For your convenience there are two models of execution:
  -- 
  -- * A simple function on IO.
  -- 
  -- * A session monad transformer, 
  -- which is an adaptation of the 'ReaderT' API over the connections pool.
  
  -- *** Simple IO
  txIO,

  -- *** Session
  Session,
  sessionInner,
  txSession,

  -- ** Transactions
  Transaction.StatementTx,
  Transaction.unitTx,
  Transaction.countTx,
  Transaction.streamTx,
  Transaction.cursorStreamTx,

  -- ** Statement Quasi-Quoter
  QQ.q,

  -- ** Error
  Transaction.Error(..),

  -- ** Results Stream
  Transaction.ResultsStream,
  Transaction.TransactionListT,

  -- ** Row parser
  RowParser.RowParser(..),
)
where

import Hasql.Prelude hiding (read)
import qualified Hasql.Backend as Backend
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Pool as Pool
import qualified Hasql.RowParser as RowParser
import qualified Hasql.QQ as QQ


-- |
-- Execute a transaction on a pool of connections.
txIO :: 
  Backend.Backend b =>
  Pool.Pool b -> Transaction.Mode -> (forall s. Transaction.Transaction b s r) -> IO r
txIO p m t =
  Pool.withConnection (\c -> Transaction.txIO m c t) p


-- * Session
-------------------------

-- |
-- A convenience monad transformer, 
-- which is just a simple wrapper around a 'ReaderT'.
type Session b =
  ReaderT (Pool.Pool b)

-- |
-- Run the session monad transformer in the inner monad.
sessionInner :: Pool.Pool b -> Session b m r -> m r
sessionInner pool reader =
  runReaderT reader pool

-- |
-- Execute a transaction in a session.
txSession :: 
  Backend.Backend b => MonadIO m =>
  Transaction.Mode -> (forall s. Transaction.Transaction b s r) -> Session b m r
txSession m t =
  ReaderT $ \p -> liftIO $ txIO p m t


