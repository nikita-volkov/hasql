module HighSQL
(
  -- * Pool
  Pool,
  Settings(..),
  withPool,
  -- * Error
  Error(..),
  -- * Transaction
  T,
  -- ** Execution
  -- |
  -- Functions for execution of transactions.
  -- They determine the transactional locking strategy of the database.
  read,
  write,
  admin,
  -- ** Statement Quasi-Quote
  q,
  -- * Results Stream
  ResultsStream,
)
where

import HighSQL.API as API
import HighSQL.QQ as QQ
