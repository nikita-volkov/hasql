-- |
-- This module defines the internal state of a database connection.
module Core.Structures.ConnectionState
  ( ConnectionState (..),
    setConnection,
    mapStatementCache,
    resetPreparedStatementsCache,
  )
where

import Core.Structures.OidCache qualified as OidCache
import Core.Structures.StatementCache qualified as StatementCache
import Platform.Prelude
import Pq qualified

-- |
-- The internal state of a database connection.
data ConnectionState = ConnectionState
  { -- | Whether prepared statements are enabled.
    preparedStatements :: Bool,
    -- | The statement cache for prepared statements.
    statementCache :: StatementCache.StatementCache,
    -- | The OID cache for type name to OID mapping.
    oidCache :: OidCache.OidCache,
    -- | The underlying database connection.
    connection :: Pq.Connection
  }

setConnection :: Pq.Connection -> ConnectionState -> ConnectionState
setConnection connection connectionState =
  connectionState {connection = connection}

mapStatementCache ::
  (StatementCache.StatementCache -> StatementCache.StatementCache) ->
  (ConnectionState -> ConnectionState)
mapStatementCache f ConnectionState {..} =
  ConnectionState
    { statementCache = f statementCache,
      ..
    }

resetPreparedStatementsCache :: ConnectionState -> ConnectionState
resetPreparedStatementsCache =
  mapStatementCache (const StatementCache.empty)
