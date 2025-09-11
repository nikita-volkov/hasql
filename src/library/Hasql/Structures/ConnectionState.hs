-- |
-- This module defines the internal state of a database connection.
module Hasql.Structures.ConnectionState
  ( ConnectionState (..),
    fromConnection,
    setIntegerDatetimes,
    setPreparedStatements,
    setStatementCache,
    setConnection,
    mapStatementCache,
    traverseStatementCache,
    resetPreparedStatementsCache,
  )
where

import Hasql.Prelude
import Hasql.Structures.StatementCache qualified as StatementCache
import Libpq qualified as Pq

-- |
-- The internal state of a database connection.
data ConnectionState = ConnectionState
  { -- | Whether prepared statements are enabled.
    preparedStatements :: Bool,
    -- | Whether integer datetimes are used.
    integerDatetimes :: Bool,
    -- | The statement cache for prepared statements.
    statementCache :: StatementCache.StatementCache,
    -- | The underlying database connection.
    connection :: Pq.Connection
  }

fromConnection :: Pq.Connection -> ConnectionState
fromConnection connection =
  ConnectionState
    { preparedStatements = False,
      integerDatetimes = False,
      statementCache = StatementCache.empty,
      connection = connection
    }

setIntegerDatetimes :: Bool -> ConnectionState -> ConnectionState
setIntegerDatetimes integerDatetimes connectionState =
  connectionState {integerDatetimes = integerDatetimes}

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

resetPreparedStatementsCache :: ConnectionState -> ConnectionState
resetPreparedStatementsCache =
  mapStatementCache (const StatementCache.empty)
