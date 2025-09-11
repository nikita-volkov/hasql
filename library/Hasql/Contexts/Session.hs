module Hasql.Contexts.Session where

import Hasql.Contexts.Command qualified as Command
import Hasql.Contexts.Pipeline qualified as Pipeline
import Hasql.Contexts.Roundtrip qualified as Roundtrip
import Hasql.Decoders.All qualified as Decoders
import Hasql.Decoders.Results qualified as ResultsDecoders
import Hasql.Encoders.All qualified as Encoders
import Hasql.Encoders.Params qualified as Encoders.Params
import Hasql.Errors
import Hasql.Prelude
import Hasql.Statement qualified as Statement
import Hasql.Structures.ConnectionState qualified as ConnectionState
import Hasql.Structures.StatementCache qualified as StatementCache

-- |
-- A batch of actions to be executed in the context of a database connection.
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
  liftInformedCommand packError roundtrip
  where
    packError =
      QueryError sql (Encoders.Params.renderReadable paramsEncoder input)

    roundtrip usePreparedStatements integerDatetimes registry = do
      registry' <-
        if usePreparedStatements && preparable
          then
            let (oidList, valueAndFormatList) = Encoders.Params.compilePreparedStatementData paramsEncoder integerDatetimes input
             in Command.prepareWithRegistry sql oidList valueAndFormatList registry
          else
            let paramsData = Encoders.Params.compileUnpreparedStatementData paramsEncoder integerDatetimes input
             in do
                  Command.sendQueryParams sql paramsData
                  pure registry
      result <- ResultsDecoders.toCommand integerDatetimes decoder
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
