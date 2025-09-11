module Hasql.Contexts.Session where

import Hasql.Contexts.Command qualified as Command
import Hasql.Contexts.ParamsEncoder qualified as ContextParams
import Hasql.Contexts.Pipeline qualified as Pipeline
import Hasql.Contexts.ResultsDecoder qualified as ContextResults
import Hasql.Contexts.Roundtrip qualified as Roundtrip
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Errors
import Hasql.Prelude
import Hasql.Statement qualified as Statement
import Hasql.Structures.ConnectionState qualified as ConnectionState
import Hasql.Structures.StatementCache qualified as StatementCache
import Libpq qualified as Pq

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

liftRoundtrip :: (CommandError -> SessionError) -> Roundtrip.Roundtrip a -> Session a
liftRoundtrip packError roundtrip = Session \connectionState -> do
  let pqConnection = ConnectionState.connection connectionState
  result <- join $ Roundtrip.run roundtrip pqConnection
  case result of
    Left err -> pure (Left (packError err), connectionState)
    Right a -> pure (Right a, connectionState)

liftInformedRoundtrip ::
  (CommandError -> SessionError) ->
  (Bool -> Bool -> StatementCache.StatementCache -> Roundtrip.Roundtrip (a, StatementCache.StatementCache)) ->
  Session a
liftInformedRoundtrip packError roundtrip = Session \connectionState -> do
  let usePreparedStatements = ConnectionState.preparedStatements connectionState
      integerDatetimes = ConnectionState.integerDatetimes connectionState
      statementCache = ConnectionState.statementCache connectionState
      pqConnection = ConnectionState.connection connectionState
  result <- join $ Roundtrip.run (roundtrip usePreparedStatements integerDatetimes statementCache) pqConnection
  case result of
    Left err -> pure (Left (packError err), connectionState)
    Right (a, newStatementCache) ->
      let newState = connectionState {ConnectionState.statementCache = newStatementCache}
       in pure (Right a, newState)

liftCommand ::
  (CommandError -> SessionError) ->
  Command.Command a ->
  Session a
liftCommand packError command =
  liftInformedCommand packError \_ _ statementCache -> (,statementCache) <$> command

liftInformedCommand ::
  (CommandError -> SessionError) ->
  (Bool -> Bool -> StatementCache.StatementCache -> Command.Command (a, StatementCache.StatementCache)) ->
  Session a
liftInformedCommand packError command = Session \connectionState -> do
  let usePreparedStatements = ConnectionState.preparedStatements connectionState
      integerDatetimes = ConnectionState.integerDatetimes connectionState
      statementCache = ConnectionState.statementCache connectionState
      pqConnection = ConnectionState.connection connectionState
  result <- Command.run (command usePreparedStatements integerDatetimes statementCache) pqConnection
  case result of
    Left err -> pure (Left (packError err), connectionState)
    Right (a, newStatementCache) ->
      let newState = connectionState {ConnectionState.statementCache = newStatementCache}
       in pure (Right a, newState)

-- |
-- Possibly a multi-statement query,
-- which however cannot be parameterized or prepared,
-- nor can any results of it be collected.
sql :: ByteString -> Session ()
sql sql =
  liftCommand (QueryError sql []) (Command.runSql sql)

-- |
-- Execute a statement by providing parameters to it.
statement :: params -> Statement.Statement params result -> Session result
statement input (Statement.Statement sql (Encoders.Params paramsEncoder) (Decoders.Result decoder) preparable) =
  liftInformedCommand packError command
  where
    packError =
      QueryError sql (ContextParams.renderReadable paramsEncoder input)

    command usePreparedStatements integerDatetimes registry = do
      registry' <-
        if usePreparedStatements && preparable
          then
            let (oidList, valueAndFormatList) = ContextParams.compilePreparedStatementData paramsEncoder integerDatetimes input
             in Command.prepareWithRegistry sql oidList valueAndFormatList registry
          else
            let paramsData = ContextParams.compileUnpreparedStatementData paramsEncoder integerDatetimes input
             in do
                  Command.sendQueryParams sql paramsData
                  pure registry
      result <- ContextResults.toCommandByIdt decoder integerDatetimes
      Command.drainResults
      pure (result, registry')

-- |
-- Execute a pipeline.
pipeline :: Pipeline.Pipeline result -> Session result
pipeline pipeline = Session \connectionState -> do
  let usePreparedStatements = ConnectionState.preparedStatements connectionState
      integerDatetimes = ConnectionState.integerDatetimes connectionState
      statementCache = ConnectionState.statementCache connectionState
      pqConnection = ConnectionState.connection connectionState
  pipelineResult <- Pipeline.run pipeline usePreparedStatements pqConnection integerDatetimes statementCache
  case pipelineResult of
    Left err -> pure (Left err, connectionState)
    Right (result, newCache) ->
      let newState = ConnectionState.setStatementCache newCache connectionState
       in pure (Right result, newState)

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
