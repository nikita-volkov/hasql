-- |
-- This module defines the internal state of a database connection.
module Core.Structures.ConnectionState
  ( ConnectionState (..),
    toStatementCache,
    fromConnection,
    setPreparedStatements,
    setStatementCache,
    setConnection,
    mapStatementCache,
    traverseStatementCache,
    reset,
  )
where

import Core.Structures.StatementCache qualified as StatementCache
import Platform.Prelude hiding (reset)
import Pq qualified

-- |
-- The internal state of a database connection.
data ConnectionState = ConnectionState
  { -- | Whether prepared statements are enabled.
    preparedStatements :: Bool,
    -- | The statement cache for prepared statements.
    statementCache :: StatementCache.StatementCache,
    -- | The underlying database connection.
    connection :: Pq.Connection
  }

toStatementCache :: ConnectionState -> StatementCache.StatementCache
toStatementCache ConnectionState {..} = statementCache

fromConnection :: Pq.Connection -> ConnectionState
fromConnection connection =
  ConnectionState
    { preparedStatements = False,
      statementCache = StatementCache.empty,
      connection = connection
    }

setPreparedStatements :: Bool -> ConnectionState -> ConnectionState
setPreparedStatements preparedStatements connectionState =
  connectionState {preparedStatements = preparedStatements}

setStatementCache :: StatementCache.StatementCache -> ConnectionState -> ConnectionState
setStatementCache statementCache connectionState =
  connectionState {statementCache = statementCache}

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

reset :: ConnectionState -> ConnectionState
reset =
  mapStatementCache (const StatementCache.empty)
