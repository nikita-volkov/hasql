-- |
-- This module defines the internal state of a database connection.
module Hasql.Engine.Structures.ConnectionState
  ( ConnectionState (..),
    toStatementCache,
    setStatementCache,
    setConnection,
    setOidCache,
    mapStatementCache,
    mapOidCache,
    traverseStatementCache,
    resetPreparedStatementsCache,
  )
where

import Hasql.Codecs.Vocab.OidCache qualified as OidCache
import Hasql.Engine.Structures.StatementCache qualified as StatementCache
import Hasql.Platform.Prelude
import Hasql.Pq qualified as Pq

-- |
-- The internal state of a database connection.
data ConnectionState = ConnectionState
  { -- | The statement cache for prepared statements.
    statementCache :: StatementCache.StatementCache,
    -- | The OID cache for type name to OID mapping.
    oidCache :: OidCache.OidCache,
    -- | The underlying database connection.
    connection :: Pq.Connection
  }

toStatementCache :: ConnectionState -> StatementCache.StatementCache
toStatementCache ConnectionState {..} = statementCache

setStatementCache :: StatementCache.StatementCache -> ConnectionState -> ConnectionState
setStatementCache statementCache connectionState =
  connectionState {statementCache = statementCache}

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

-- |
-- Bring the cache back to its initial state, for when the server side has just
-- been cleared with a @DEALLOCATE ALL@.
resetPreparedStatementsCache :: ConnectionState -> ConnectionState
resetPreparedStatementsCache =
  mapStatementCache StatementCache.reset
