-- |
-- This module defines the internal state of a database connection.
module Hasql.Structures.ConnectionState
  ( ConnectionState (..),
  )
where

import Hasql.LibPq14 qualified as Pq
import Hasql.Prelude
import Hasql.Structures.StatementCache qualified as StatementCache

-- |
-- The internal state of a database connection.
data ConnectionState = ConnectionState
  { -- | Whether prepared statements are enabled.
    preparedStatements :: !Bool,
    -- | Whether integer datetimes are used.
    integerDatetimes :: !Bool,
    -- | The statement cache for prepared statements.
    statementCache :: !StatementCache.StatementCache,
    -- | The underlying database connection.
    connection :: !Pq.Connection
  }
