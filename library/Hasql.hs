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
  withoutLocking,
  read,
  write,
  -- ** Transactions
  Transaction.execute,
  Transaction.executeAndCount,
  Transaction.executeAndFetch,
  Transaction.executeAndFetchWithCursor,
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

import Hasql.Prelude hiding (read)
import qualified Hasql.Backend as Backend
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Pool as Pool
import qualified Hasql.RowParser as RowParser
import qualified Hasql.QQ as QQ


withoutLocking :: 
  Backend.Backend b => 
  (forall s. Transaction.Transaction b Transaction.WithoutLocking s r) -> Pool.Pool b -> IO r
withoutLocking t =
  Pool.withConnection (Transaction.runWithoutLocking t)

read ::
  Backend.Backend b => 
  Backend.IsolationLevel -> (forall s. Transaction.Transaction b Transaction.Read s r) -> Pool.Pool b -> IO r
read i t =
  Pool.withConnection (Transaction.runRead i t)

write ::
  Backend.Backend b => 
  Backend.IsolationLevel -> (forall s. Transaction.Transaction b Transaction.Write s r) -> Pool.Pool b -> IO r
write i t =
  Pool.withConnection (Transaction.runWrite i t)
