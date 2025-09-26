-- |
-- This module defines the internal state of a database connection.
module Core.Structures.ConnectionState
  ( ConnectionState (..),
    toStatementCache,
    toOIDCache,
    fromConnection,
    setPreparedStatements,
    setStatementCache,
    setOIDCache,
    setConnection,
    mapStatementCache,
    mapOIDCache,
    traverseStatementCache,
    traverseOIDCache,
    resetPreparedStatementsCache,
  )
where

import Core.Structures.OIDCache qualified as OIDCache
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
    -- | The OID cache for custom types.
    oidCache :: OIDCache.OIDCache,
    -- | The underlying database connection.
    connection :: Pq.Connection
  }

toStatementCache :: ConnectionState -> StatementCache.StatementCache
toStatementCache ConnectionState {..} = statementCache

toOIDCache :: ConnectionState -> OIDCache.OIDCache
toOIDCache ConnectionState {..} = oidCache

fromConnection :: Pq.Connection -> ConnectionState
fromConnection connection =
  ConnectionState
    { preparedStatements = False,
      statementCache = StatementCache.empty,
      oidCache = OIDCache.empty,
      connection = connection
    }

setPreparedStatements :: Bool -> ConnectionState -> ConnectionState
setPreparedStatements preparedStatements connectionState =
  connectionState {preparedStatements = preparedStatements}

setStatementCache :: StatementCache.StatementCache -> ConnectionState -> ConnectionState
setStatementCache statementCache connectionState =
  connectionState {statementCache = statementCache}

setOIDCache :: OIDCache.OIDCache -> ConnectionState -> ConnectionState
setOIDCache oidCache connectionState =
  connectionState {oidCache = oidCache}

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

mapOIDCache ::
  (OIDCache.OIDCache -> OIDCache.OIDCache) ->
  (ConnectionState -> ConnectionState)
mapOIDCache f ConnectionState {..} =
  ConnectionState
    { oidCache = f oidCache,
      ..
    }

traverseStatementCache ::
  (Functor f) =>
  (StatementCache.StatementCache -> f StatementCache.StatementCache) ->
  (ConnectionState -> f ConnectionState)
traverseStatementCache f ConnectionState {..} =
  fmap
    ( \newStatementCache ->
        ConnectionState
          { statementCache = newStatementCache,
            ..
          }
    )
    (f statementCache)

traverseOIDCache ::
  (Functor f) =>
  (OIDCache.OIDCache -> f OIDCache.OIDCache) ->
  (ConnectionState -> f ConnectionState)
traverseOIDCache f ConnectionState {..} =
  fmap
    ( \newOIDCache ->
        ConnectionState
          { oidCache = newOIDCache,
            ..
          }
    )
    (f oidCache)

resetPreparedStatementsCache :: ConnectionState -> ConnectionState
resetPreparedStatementsCache =
  mapStatementCache (const StatementCache.empty)
