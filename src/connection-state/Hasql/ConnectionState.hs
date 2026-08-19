-- |
-- This module defines the internal state of a database connection.
module Hasql.ConnectionState
  ( ConnectionState (..),
    toStatementCache,
    fromConnection,
    setPreparedStatements,
    setStatementCache,
    setConnection,
    setDead,
    setOidCache,
    mapStatementCache,
    mapOidCache,
    traverseStatementCache,
    resetPreparedStatementsCache,
  )
where

import Hasql.ConnectionState.OidCache qualified as OidCache
import Hasql.ConnectionState.StatementCache qualified as StatementCache
import Hasql.Platform.Prelude
import Pqi qualified as Pq

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
    connection :: Pq.Connection,
    -- | Whether the driver has given up on the connection.
    --
    -- Set when an operation fails in a way that leaves nothing to be assumed
    -- about the connection's protocol state (see
    -- 'Hasql.Engine.Errors.connectionIsSpent'). Sessions short-circuit on
    -- it, and 'Hasql.Connection.use' finishes the connection when it comes
    -- back set.
    --
    -- It is state rather than a property of the error a session returns
    -- because a session is a 'Control.Monad.Except.MonadError' and can catch
    -- the failure and carry on - or catch it and succeed, in which case the
    -- error never reaches 'Hasql.Connection.use' at all, and without this
    -- the connection would be handed back for reuse in a state libpq
    -- refuses to serve.
    dead :: Bool
  }

toStatementCache :: ConnectionState -> StatementCache.StatementCache
toStatementCache ConnectionState {..} = statementCache

fromConnection :: Pq.Connection -> ConnectionState
fromConnection connection =
  ConnectionState
    { preparedStatements = False,
      statementCache = StatementCache.empty,
      oidCache = OidCache.empty,
      connection = connection,
      dead = False
    }

setPreparedStatements :: Bool -> ConnectionState -> ConnectionState
setPreparedStatements preparedStatements connectionState =
  connectionState {preparedStatements = preparedStatements}

setStatementCache :: StatementCache.StatementCache -> ConnectionState -> ConnectionState
setStatementCache statementCache connectionState =
  connectionState {statementCache = statementCache}

setDead :: ConnectionState -> ConnectionState
setDead connectionState =
  connectionState {dead = True}

setConnection :: Pq.Connection -> ConnectionState -> ConnectionState
setConnection connection connectionState =
  connectionState {connection = connection}

setOidCache :: OidCache.OidCache -> ConnectionState -> ConnectionState
setOidCache oidCache connectionState =
  connectionState {oidCache}

mapStatementCache ::
  (StatementCache.StatementCache -> StatementCache.StatementCache) ->
  (ConnectionState -> ConnectionState)
mapStatementCache f ConnectionState {..} =
  ConnectionState
    { statementCache = f statementCache,
      ..
    }

mapOidCache ::
  (OidCache.OidCache -> OidCache.OidCache) ->
  (ConnectionState -> ConnectionState)
mapOidCache f ConnectionState {..} =
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

resetPreparedStatementsCache :: ConnectionState -> ConnectionState
resetPreparedStatementsCache =
  mapStatementCache (const StatementCache.empty)
