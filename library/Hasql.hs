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
  txIO,

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

