module Hasql.Session.Core where

import Control.Monad.State.Class (MonadState (..), gets, modify)
import Hasql.Connection.Core qualified as Connection
import Hasql.Contexts.Command qualified as Command
import Hasql.Contexts.Roundtrip qualified as Roundtrip
import Hasql.Decoders.All qualified as Decoders
import Hasql.Decoders.Results qualified as ResultsDecoders
import Hasql.Encoders.All qualified as Encoders
import Hasql.Encoders.Params qualified as Encoders.Params
import Hasql.Errors
import Hasql.LibPq14 qualified as Pq
import Hasql.Pipeline.Core qualified as Pipeline
import Hasql.Prelude
import Hasql.Statement qualified as Statement
import Hasql.Structures.ConnectionState qualified as ConnectionState
import Hasql.Structures.StatementCache qualified as StatementCache

-- |
-- A batch of actions to be executed in the context of a database connection.
newtype Session a
  = Session (StateT ConnectionState.ConnectionState (ExceptT SessionError IO) a)
  deriving (Functor, Applicative, Monad, MonadError SessionError, MonadIO, MonadState ConnectionState.ConnectionState)

-- |
-- Executes a bunch of commands on the provided connection.
run :: Session a -> Connection.Connection -> IO (Either SessionError a)
run (Session session) (Connection.Connection mvar) =
  mask $ \restore -> onException (restore main) handler
  where
    main = modifyMVar mvar \initialState -> do
      result <- runExceptT $ runStateT session initialState
      case result of
        Left err -> pure (initialState, Left err)
        Right (a, newState) -> pure (newState, Right a)
    handler = modifyMVar_ mvar \state -> do
      let pqConn = ConnectionState.connection state
      Pq.transactionStatus pqConn >>= \case
        Pq.TransIdle -> pure state
        _ -> Pq.reset pqConn >> pure state

liftRoundtrip :: (CommandError -> SessionError) -> Roundtrip.Roundtrip a -> Session a
liftRoundtrip packError roundtrip = Session do
  pqConnection <- gets ConnectionState.connection
  result <- liftIO $ join $ Roundtrip.run roundtrip pqConnection
  either (throwError . packError) pure result

liftInformedRoundtrip ::
  (CommandError -> SessionError) ->
  ( Bool ->
    Bool ->
    StatementCache.StatementCache ->
    Roundtrip.Roundtrip (a, StatementCache.StatementCache)
  ) ->
  Session a
liftInformedRoundtrip packError roundtrip = Session do
  usePreparedStatements <- gets ConnectionState.preparedStatements
  integerDatetimes <- gets ConnectionState.integerDatetimes
  statementCache <- gets ConnectionState.statementCache
  pqConnection <- gets ConnectionState.connection
  result <- liftIO $ join $ Roundtrip.run (roundtrip usePreparedStatements integerDatetimes statementCache) pqConnection
  case result of
    Left err -> throwError (packError err)
    Right (a, newStatementCache) -> do
      modify \s -> s {ConnectionState.statementCache = newStatementCache}
      pure a

liftCommand ::
  (CommandError -> SessionError) ->
  Command.Command a ->
  Session a
liftCommand packError command =
  liftInformedCommand packError \_ _ statementCache -> (,statementCache) <$> command

liftInformedCommand ::
  (CommandError -> SessionError) ->
  ( Bool ->
    Bool ->
    StatementCache.StatementCache ->
    Command.Command (a, StatementCache.StatementCache)
  ) ->
  Session a
liftInformedCommand packError command = Session do
  usePreparedStatements <- gets ConnectionState.preparedStatements
  integerDatetimes <- gets ConnectionState.integerDatetimes
  statementCache <- gets ConnectionState.statementCache
  pqConnection <- gets ConnectionState.connection
  result <- liftIO $ Command.run (command usePreparedStatements integerDatetimes statementCache) pqConnection
  case result of
    Left err -> throwError (packError err)
    Right (a, newStatementCache) -> do
      modify \s -> s {ConnectionState.statementCache = newStatementCache}
      pure a

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
      registry <-
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
      pure (result, registry)

-- |
-- Execute a pipeline.
pipeline :: Pipeline.Pipeline result -> Session result
pipeline pipeline = Session do
  usePreparedStatements <- gets ConnectionState.preparedStatements
  integerDatetimes <- gets ConnectionState.integerDatetimes
  statementCache <- gets ConnectionState.statementCache
  pqConnection <- gets ConnectionState.connection
  pipelineResult <- liftIO $ Pipeline.run pipeline usePreparedStatements pqConnection integerDatetimes statementCache
  case pipelineResult of
    Left err -> throwError err
    Right (result, newCache) -> do
      modify \s -> s {ConnectionState.statementCache = newCache}
      pure result
