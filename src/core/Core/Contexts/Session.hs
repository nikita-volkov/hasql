module Core.Contexts.Session where

import Core.Contexts.ParamsEncoder qualified as ParamsEncoder
import Core.Contexts.Pipeline qualified as Pipeline
import Core.Errors
import Core.Structures.ConnectionState qualified as ConnectionState
import Hipq.ResultDecoder qualified
import Hipq.Roundtrip qualified
import Platform.Prelude
import Pq qualified

-- |
-- A sequence of operations to be executed in the context of a single database connection with exclusive access to it.
--
-- Construct sessions using helpers in this module such as
-- 'statement', 'pipeline' and 'sql', or use 'onLibpqConnection' for a low-level
-- escape hatch.
--
-- To actually execute a 'Session' use 'Hasql.Connection.use', which manages
-- concurrent access to the shared connection state and returns either a
-- 'SessionError' or the result:
--
-- > result <- Hasql.Connection.use connection mySession
--
-- Note: while most session errors are returned as values, user code executed
-- inside a session may still throw exceptions; in that case the driver will
-- reset the connection to a clean state.
newtype Session a
  = Session (ConnectionState.ConnectionState -> IO (Either SessionError a, ConnectionState.ConnectionState))
  deriving
    (Functor, Applicative, Monad, MonadError SessionError, MonadIO)
    via (ExceptT SessionError (StateT ConnectionState.ConnectionState IO))

run :: Session a -> ConnectionState.ConnectionState -> IO (Either SessionError a, ConnectionState.ConnectionState)
run (Session session) connectionState = session connectionState

-- |
-- Possibly a multi-statement query,
-- which however cannot be parameterized or prepared,
-- nor can any results of it be collected.
sql :: ByteString -> Session ()
sql sql =
  let context =
        StatementErrorContext sql [] False
   in Session \connectionState -> do
        let connection = ConnectionState.connection connectionState
        result <- Hipq.Roundtrip.toSerialIO (Hipq.Roundtrip.query context sql) connection
        case result of
          Left err -> case err of
            Hipq.Roundtrip.ClientError context details -> do
              Pq.reset connection
              pure
                ( Left (addContextToCommandError context (ClientError details)),
                  ConnectionState.resetPreparedStatementsCache connectionState
                )
            Hipq.Roundtrip.ServerError recvError ->
              pure
                ( Left (adaptServerError recvError),
                  connectionState
                )
          Right () ->
            pure
              ( Right (),
                connectionState
              )

-- |
-- Execute a statement by providing parameters to it.
statement ::
  forall params result.
  ByteString ->
  ParamsEncoder.ParamsEncoder params ->
  Hipq.ResultDecoder.ResultDecoder result ->
  Bool ->
  params ->
  Session result
statement sql paramsEncoder decoder preparable params =
  pipeline do
    Pipeline.statement sql paramsEncoder decoder preparable params

-- |
-- Execute a pipeline.
pipeline :: Pipeline.Pipeline result -> Session result
pipeline pipeline = Session \connectionState -> do
  let usePreparedStatements = ConnectionState.preparedStatements connectionState
      statementCache = ConnectionState.statementCache connectionState
      pqConnection = ConnectionState.connection connectionState
  (result, newCache) <- Pipeline.run pipeline usePreparedStatements pqConnection statementCache
  let newState = ConnectionState.setStatementCache newCache connectionState
  pure (result, newState)

-- |
-- Execute an operation on the raw libpq connection possibly producing an error and updating the connection.
-- This is a low-level escape hatch for custom integrations.
--
-- You can supply a new connection in the result to replace it in the running Hasql connection.
-- The responsibility to close the old libpq connection is on you.
-- Otherwise, just return the same connection you've received.
--
-- Producing a 'Left' value will cause the session to fail with the given error.
-- Regardless of success or failure, the connection will be replaced with the one you return.
--
-- Throwing exceptions is okay. It will lead to the connection getting reset.
onLibpqConnection :: (Pq.Connection -> IO (Either SessionError a, Pq.Connection)) -> Session a
onLibpqConnection f = Session \connectionState -> do
  let pqConnection = ConnectionState.connection connectionState
  (result, newConnection) <- f pqConnection
  let newState = ConnectionState.setConnection newConnection connectionState
  pure (result, newState)
