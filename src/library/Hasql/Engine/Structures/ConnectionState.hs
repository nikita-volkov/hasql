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

import Hasql.Driver.Interface qualified as Interface
import Hasql.Engine.Structures.OidCache qualified as OidCache
import Hasql.Engine.Structures.StatementCache qualified as StatementCache
import Hasql.Platform.Prelude

-- |
-- The internal state of a database connection.
data ConnectionState conn result = ConnectionState
  { -- | The driver used for all database operations.
    driver :: Interface.Driver conn result,
    -- | Whether prepared statements are enabled.
    preparedStatements :: Bool,
    -- | The statement cache for prepared statements.
    statementCache :: StatementCache.StatementCache,
    -- | The OID cache for type name to OID mapping.
    oidCache :: OidCache.OidCache,
    -- | The underlying database connection.
    connection :: conn
  }

toStatementCache :: ConnectionState conn result -> StatementCache.StatementCache
toStatementCache ConnectionState {..} = statementCache

fromConnection :: Interface.Driver conn result -> conn -> ConnectionState conn result
fromConnection drv connection =
  ConnectionState
    { driver = drv,
      preparedStatements = False,
      statementCache = StatementCache.empty,
      oidCache = OidCache.empty,
      connection = connection
    }

setPreparedStatements :: Bool -> ConnectionState conn result -> ConnectionState conn result
setPreparedStatements preparedStatements connectionState =
  connectionState {preparedStatements = preparedStatements}

setStatementCache :: StatementCache.StatementCache -> ConnectionState conn result -> ConnectionState conn result
setStatementCache statementCache connectionState =
  connectionState {statementCache = statementCache}

setConnection :: conn -> ConnectionState conn result -> ConnectionState conn result
setConnection connection connectionState =
  connectionState {connection = connection}

setOidCache :: OidCache.OidCache -> ConnectionState conn result -> ConnectionState conn result
setOidCache oidCache connectionState =
  connectionState {oidCache}

mapStatementCache ::
  (StatementCache.StatementCache -> StatementCache.StatementCache) ->
  (ConnectionState conn result -> ConnectionState conn result)
mapStatementCache f ConnectionState {..} =
  ConnectionState
    { statementCache = f statementCache,
      ..
    }

mapOidCache ::
  (OidCache.OidCache -> OidCache.OidCache) ->
  (ConnectionState conn result -> ConnectionState conn result)
mapOidCache f ConnectionState {..} =
  ConnectionState
    { oidCache = f oidCache,
      ..
    }

traverseStatementCache ::
  (Functor f) =>
  (StatementCache.StatementCache -> f StatementCache.StatementCache) ->
  (ConnectionState conn result -> f (ConnectionState conn result))
traverseStatementCache f ConnectionState {..} =
  fmap
    ( \newStatementCache ->
        ConnectionState
          { statementCache = newStatementCache,
            ..
          }
    )
    (f statementCache)

resetPreparedStatementsCache :: ConnectionState conn result -> ConnectionState conn result
resetPreparedStatementsCache =
  mapStatementCache (const StatementCache.empty)
