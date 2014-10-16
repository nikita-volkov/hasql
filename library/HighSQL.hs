module HighSQL
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
  runWithoutLocking,
  runRead,
  runWrite,
  -- ** Transactions
  Transaction.modify,
  Transaction.modifyAndGenerate,
  Transaction.modifyAndCount,
  Transaction.select,
  Transaction.selectWithCursor,
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
  Transaction.ModificationPrivilege,
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

import HighSQL.Prelude
import qualified HighSQL.Backend as Backend
import qualified HighSQL.Transaction as Transaction
import qualified HighSQL.Pool as Pool
import qualified HighSQL.RowParser as RowParser
import qualified HighSQL.QQ as QQ


runWithoutLocking :: 
  Backend.Backend b => 
  (forall s. Transaction.Transaction b Transaction.WithoutLocking s r) -> Pool.Pool b -> IO r
runWithoutLocking t =
  Pool.withConnection (Transaction.runWithoutLocking t)

runRead ::
  Backend.Backend b => 
  Backend.IsolationLevel -> (forall s. Transaction.Transaction b Transaction.Read s r) -> Pool.Pool b -> IO r
runRead i t =
  Pool.withConnection (Transaction.runRead i t)

runWrite ::
  Backend.Backend b => 
  Backend.IsolationLevel -> (forall s. Transaction.Transaction b Transaction.Write s r) -> Pool.Pool b -> IO r
runWrite i t =
  Pool.withConnection (Transaction.runWrite i t)
