module Hasql
(
  -- * Pool
  Pool.Pool,
  Pool.Settings(..),
  Pool.withPool,
  -- * Transaction
  Transaction.Transaction,
  -- ** Execution
  -- |
  -- Functions for execution of transactions.
  -- They determine the transactional locking strategy of the database.
  TransactionRunner,
  withoutLocking,
  read,
  write,
  -- ** Transactions
  Transaction.StatementRunner,
  Transaction.unitTx,
  Transaction.countTx,
  Transaction.streamTx,
  Transaction.cursorStreamTx,
  -- ** Statement Quasi-Quoter
  QQ.q,
  -- ** Error
  Transaction.Error(..),
  -- ** Isolation
  Backend.IsolationLevel(..),
  -- ** Locking Levels
  Transaction.WithoutLocking,
  Transaction.Read,
  Transaction.Write,
  -- ** Privileges
  Transaction.CursorsPrivilege,
  Transaction.WritingPrivilege,
  -- ** Results Stream
  Transaction.ResultsStream,
  Transaction.TransactionListT,
  -- ** Backend
  Backend.Backend,
  Backend.Mapping,
  -- ** Row parser
  RowParser.RowParser,
)
where

import Hasql.Prelude hiding (read)
import qualified Hasql.Backend as Backend
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Pool as Pool
import qualified Hasql.RowParser as RowParser
import qualified Hasql.QQ as QQ


type TransactionRunner l =
  forall b r. Backend.Backend b =>
  (forall s. Transaction.Transaction b l s r) -> Pool.Pool b -> IO r

withoutLocking :: TransactionRunner Transaction.WithoutLocking
withoutLocking t =
  Pool.withConnection (Transaction.runWithoutLocking t)

read :: Backend.IsolationLevel -> TransactionRunner Transaction.Read
read i t =
  Pool.withConnection (Transaction.runRead i t)

write :: Backend.IsolationLevel -> TransactionRunner Transaction.Write
write i t =
  Pool.withConnection (Transaction.runWrite i t)




