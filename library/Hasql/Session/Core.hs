module Hasql.Session.Core where

import Hasql.Connection.Core qualified as Connection
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

-- |
-- Possibly a multi-statement query,
-- which however cannot be parameterized or prepared,
-- nor can any results of it be collected.
sql :: ByteString -> Session ()
sql sql =
  liftRoundtrip (QueryError sql []) (Roundtrip.runSql sql)

-- |
-- Execute a statement by providing parameters to it.
statement :: params -> Statement.Statement params result -> Session result
statement input (Statement.Statement template (Encoders.Params paramsEncoder) (Decoders.Result decoder) preparable) =
  Session
    $ ReaderT
    $ \(Connection.Connection usePreparedStatements pqConnectionRef integerDatetimes registry) ->
      ExceptT
        $ fmap (first (QueryError template (Encoders.Params.renderReadable paramsEncoder input)))
        $ withMVar pqConnectionRef
        $ \pqConnection -> do
          r1 <-
            if usePreparedStatements && preparable
              then runExceptT $ do
                let (oidList, valueAndFormatList) = Encoders.Params.compilePreparedStatementData paramsEncoder integerDatetimes input
                key <-
                  ExceptT
                    $ PreparedStatementRegistry.update
                      (PreparedStatementRegistry.LocalKey template oidList)
                      ( \k -> do
                          recv <- Roundtrip.run (Roundtrip.prepare k template oidList) pqConnection
                          result <- recv
                          case result of
                            Left e -> pure (False, Left e)
                            Right _ -> pure (True, Right k)
                      )
                      (\k -> pure (Right k))
                      registry
                ExceptT $ do
                  sent <- Pq.sendQueryPrepared pqConnection key valueAndFormatList Pq.Binary
                  if sent
                    then pure (Right ())
                    else fmap (Left . ClientError) (Pq.errorMessage pqConnection)
              else do
                let paramsData = Encoders.Params.compileUnpreparedStatementData paramsEncoder integerDatetimes input
                Pq.sendQueryParams pqConnection template paramsData Pq.Binary >>= \sent ->
                  if sent
                    then pure (Right ())
                    else fmap (Left . ClientError) (Pq.errorMessage pqConnection)
          r2 <-
            (<*)
              <$> ResultsDecoders.run decoder pqConnection integerDatetimes
              <*> ResultsDecoders.run ResultsDecoders.dropRemainders pqConnection integerDatetimes
          return $ r1 *> r2

-- |
-- Execute a pipeline.
pipeline :: Pipeline.Pipeline result -> Session result
pipeline pipeline =
  Session $ ReaderT \(Connection.Connection usePreparedStatements pqConnectionRef integerDatetimes registry) ->
    ExceptT $ withMVar pqConnectionRef \pqConnection ->
      Pipeline.run pipeline usePreparedStatements pqConnection registry integerDatetimes
