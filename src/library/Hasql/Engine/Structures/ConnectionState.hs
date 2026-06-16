-- |
-- This module defines the internal state of a database connection.
module Hasql.Engine.Structures.ConnectionState
  ( ConnectionState (..),
    toStatementCache,
    fromConnection,
    setPreparedStatements,
    setStatementCache,
    setConnection,
    setOidCache,
    mapStatementCache,
    mapOidCache,
    traverseStatementCache,
    resetPreparedStatementsCache,
  )
where

import Hasql.Engine.Structures.OidCache qualified as OidCache
import Hasql.Engine.Structures.StatementCache qualified as StatementCache
import Hasql.Platform.Prelude
import Pqi qualified

-- |
-- The internal state of a database connection, parametric over the connection type @c@.
data ConnectionState c = ConnectionState
  { -- | Whether prepared statements are enabled.
    preparedStatements :: Bool,
    -- | The statement cache for prepared statements.
    statementCache :: StatementCache.StatementCache,
    -- | The OID cache for type name to OID mapping.
    oidCache :: OidCache.OidCache,
    -- | The underlying database connection.
    connection :: c
  }

toStatementCache :: ConnectionState c -> StatementCache.StatementCache
toStatementCache ConnectionState {..} = statementCache

fromConnection :: (Pqi.IsConnection c) => c -> ConnectionState c
fromConnection connection =
  ConnectionState
    { preparedStatements = False,
      statementCache = StatementCache.empty,
      oidCache = OidCache.empty,
      connection = connection
    }

setPreparedStatements :: Bool -> ConnectionState c -> ConnectionState c
setPreparedStatements preparedStatements connectionState =
  connectionState {preparedStatements = preparedStatements}

setStatementCache :: StatementCache.StatementCache -> ConnectionState c -> ConnectionState c
setStatementCache statementCache connectionState =
  connectionState {statementCache = statementCache}

setConnection :: c -> ConnectionState c -> ConnectionState c
setConnection connection connectionState =
  connectionState {connection = connection}

setOidCache :: OidCache.OidCache -> ConnectionState c -> ConnectionState c
setOidCache oidCache connectionState =
  connectionState {oidCache}

mapStatementCache ::
  (StatementCache.StatementCache -> StatementCache.StatementCache) ->
  (ConnectionState c -> ConnectionState c)
mapStatementCache f ConnectionState {..} =
  ConnectionState
    { statementCache = f statementCache,
      ..
    }

mapOidCache ::
  (OidCache.OidCache -> OidCache.OidCache) ->
  (ConnectionState c -> ConnectionState c)
mapOidCache f ConnectionState {..} =
  ConnectionState
    { oidCache = f oidCache,
      ..
    }

traverseStatementCache ::
  (Functor f) =>
  (StatementCache.StatementCache -> f StatementCache.StatementCache) ->
  (ConnectionState c -> f (ConnectionState c))
traverseStatementCache f ConnectionState {..} =
  fmap
    ( \newStatementCache ->
        ConnectionState
          { statementCache = newStatementCache,
            ..
          }
    )
    (f statementCache)

resetPreparedStatementsCache :: ConnectionState c -> ConnectionState c
resetPreparedStatementsCache =
  mapStatementCache (const StatementCache.empty)
