module Hasql.Session.Core where

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
import Hasql.PreparedStatementRegistry qualified as PreparedStatementRegistry
import Hasql.Statement qualified as Statement
import Hasql.Structures.StatementCache qualified as PureStatementRegistry

-- |
-- A batch of actions to be executed in the context of a database connection.
newtype Session a
  = Session (ReaderT Connection.Connection (ExceptT SessionError IO) a)
  deriving (Functor, Applicative, Monad, MonadError SessionError, MonadIO, MonadReader Connection.Connection)

-- |
-- Executes a bunch of commands on the provided connection.
run :: Session a -> Connection.Connection -> IO (Either SessionError a)
run (Session impl) connection =
  mask $ \restore -> onException (restore main) handler
  where
    main =
      runExceptT $ runReaderT impl connection
    handler =
      case connection of
        Connection.Connection _ pqConnVar _ registry ->
          withMVar pqConnVar \pqConn ->
            Pq.transactionStatus pqConn >>= \case
              Pq.TransIdle -> pure ()
              _ -> do
                PreparedStatementRegistry.reset registry
                Pq.reset pqConn

liftRoundtrip :: (CommandError -> SessionError) -> Roundtrip.Roundtrip a -> Session a
liftRoundtrip packError roundtrip =
  Session do
    ReaderT \(Connection.Connection _ pqConnectionRef _ _) ->
      ExceptT do
        withMVar pqConnectionRef \pqConnection -> do
          recv <- Roundtrip.run roundtrip pqConnection
          first packError <$> recv

liftInformedRoundtrip ::
  (CommandError -> SessionError) ->
  ( Bool ->
    Bool ->
    PureStatementRegistry.StatementCache ->
    Roundtrip.Roundtrip (a, PureStatementRegistry.StatementCache)
  ) ->
  Session a
liftInformedRoundtrip packError roundtrip =
  Session do
    ReaderT \(Connection.Connection usePreparedStatements pqConnectionRef integerDatetimes statementRegistry) ->
      ExceptT do
        withMVar pqConnectionRef \pqConnection -> do
          statementStatementCache <- PreparedStatementRegistry.readPureState statementRegistry
          recv <- Roundtrip.run (roundtrip usePreparedStatements integerDatetimes statementStatementCache) pqConnection
          result <- recv
          case result of
            Left err -> pure (Left (packError err))
            Right (result, statementStatementCache) -> do
              -- Update the statement registry state after successful execution
              PreparedStatementRegistry.writePureState statementRegistry statementStatementCache
              pure (Right result)

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
    PureStatementRegistry.StatementCache ->
    Command.Command (a, PureStatementRegistry.StatementCache)
  ) ->
  Session a
liftInformedCommand packError command =
  Session do
    ReaderT \(Connection.Connection usePreparedStatements pqConnectionRef idt statementRegistry) ->
      ExceptT do
        withMVar pqConnectionRef \pqConnection -> do
          statementStatementCache <- PreparedStatementRegistry.readPureState statementRegistry
          result <- Command.run (command usePreparedStatements idt statementStatementCache) pqConnection
          case result of
            Left err -> pure (Left (packError err))
            Right (result, statementStatementCache) -> do
              -- Update the statement registry state after successful execution
              PreparedStatementRegistry.writePureState statementRegistry statementStatementCache
              pure (Right result)

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
pipeline pipeline =
  Session $ ReaderT \(Connection.Connection usePreparedStatements pqConnectionRef integerDatetimes registry) ->
    ExceptT $ withMVar pqConnectionRef \pqConnection -> do
      statementCache <- PreparedStatementRegistry.readPureState registry
      pipelineResult <- Pipeline.run pipeline usePreparedStatements pqConnection integerDatetimes statementCache
      case pipelineResult of
        Left err -> pure (Left err)
        Right (result, newCache) -> do
          PreparedStatementRegistry.writePureState registry newCache
          pure (Right result)
