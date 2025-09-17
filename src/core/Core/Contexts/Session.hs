module Core.Contexts.Session where

import Core.Contexts.Command qualified as Command
import Core.Contexts.ParamsEncoder qualified as ParamsEncoder
import Core.Contexts.Pipeline qualified as Pipeline
import Core.Contexts.ResultConsumer qualified as ResultConsumer
import Core.Errors
import Core.Structures.ConnectionState qualified as ConnectionState
import Core.Structures.StatementCache qualified as StatementCache
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

liftCommand ::
  (CommandError -> SessionError) ->
  Command.Command a ->
  Session a
liftCommand packError command =
  liftInformedCommand packError \_ statementCache -> (,statementCache) <$> command

liftInformedCommand ::
  (CommandError -> SessionError) ->
  (Bool -> StatementCache.StatementCache -> Command.Command (a, StatementCache.StatementCache)) ->
  Session a
liftInformedCommand packError command = Session \connectionState -> do
  let usePreparedStatements = ConnectionState.preparedStatements connectionState
      statementCache = ConnectionState.statementCache connectionState
      pqConnection = ConnectionState.connection connectionState
  result <- Command.run (command usePreparedStatements statementCache) pqConnection
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
statement ::
  forall params result.
  ByteString -> ParamsEncoder.ParamsEncoder params -> ResultConsumer.ResultConsumer result -> Bool -> params -> Session result
statement sql paramsEncoder decoder preparable params =
  do
    prepared <- prepare
    case prepared of
      Just (key, valueAndFormatList) ->
        executePrepared key valueAndFormatList
      Nothing ->
        executeUnprepared
  where
    packError =
      QueryError sql (ParamsEncoder.renderReadable paramsEncoder params)

    prepare :: Session (Maybe (ByteString, [Maybe (ByteString, Pq.Format)]))
    prepare =
      liftInformedCommand packError \usePreparedStatements statementCache -> do
        if usePreparedStatements && preparable
          then
            let (oidList, valueAndFormatList) = ParamsEncoder.compilePreparedStatementData paramsEncoder params
             in Command.prepareWithCache sql oidList statementCache
                  <&> \(key, statementCache) -> (Just (key, valueAndFormatList), statementCache)
          else
            pure (Nothing, statementCache)

    executePrepared :: ByteString -> [Maybe (ByteString, Pq.Format)] -> Session result
    executePrepared key valueAndFormatList =
      liftInformedCommand packError \_usePreparedStatements statementCache -> do
        Command.sendQueryPrepared key valueAndFormatList
        result <- Command.consumeResult decoder
        Command.drainResults
        pure (result, statementCache)

    executeUnprepared :: Session result
    executeUnprepared =
      liftInformedCommand packError \_ statementCache -> do
        let paramsData = ParamsEncoder.compileUnpreparedStatementData paramsEncoder params
        Command.sendQueryParams sql paramsData
        result <- Command.consumeResult decoder
        Command.drainResults
        pure (result, statementCache)

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
