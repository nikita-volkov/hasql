module Hasql.Session.Core where

import Hasql.Connection.Core qualified as Connection
import Hasql.Decoders.All qualified as Decoders
import Hasql.Decoders.Result qualified as Decoders.Result
import Hasql.Decoders.Results qualified as Decoders.Results
import Hasql.Encoders.All qualified as Encoders
import Hasql.Encoders.Params qualified as Encoders.Params
import Hasql.Errors
import Hasql.IO qualified as IO
import Hasql.LibPq14 qualified as Pq
import Hasql.LibPq14 qualified as LibPQ
import Hasql.Pipeline.Core qualified as Pipeline
import Hasql.PostgresTypeInfo qualified as PTI
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
        Connection.Connection _ pqConnVar _ registry _ ->
          withMVar pqConnVar \pqConn ->
            Pq.transactionStatus pqConn >>= \case
              Pq.TransIdle -> pure ()
              _ -> do
                PreparedStatementRegistry.reset registry
                Pq.reset pqConn

-- |
-- Possibly a multi-statement query,
-- which however cannot be parameterized or prepared,
-- nor can any results of it be collected.
sql :: ByteString -> Session ()
sql sql =
  Session
    $ ReaderT
    $ \(Connection.Connection _ pqConnectionRef integerDatetimes _ _) ->
      ExceptT
        $ fmap (first (QueryError sql []))
        $ withMVar pqConnectionRef
        $ \pqConnection -> do
          r1 <- IO.sendNonparametricStatement pqConnection sql
          r2 <- IO.getResults pqConnection integerDatetimes decoder
          return $ r1 *> r2
  where
    decoder =
      Decoders.Results.single Decoders.Result.noResult

-- |
-- Execute a statement by providing parameters to it, with automatic OID resolution for named types.
statementWithOidResolution :: params -> Statement.Statement params result -> Session result
statementWithOidResolution input stmt = do
  resolvedStmt <- resolveStatementOids stmt
  statement input resolvedStmt

-- |
-- Resolve OIDs for any named types in a statement's encoders and decoders.
resolveStatementOids :: Statement.Statement params result -> Session (Statement.Statement params result)
resolveStatementOids (Statement.Statement template (Encoders.Params paramsEncoder) (Decoders.Result decoder) preparable) = do
  -- For now, just return the original statement
  -- TODO: Implement actual OID resolution by traversing encoders/decoders
  pure (Statement.Statement template (Encoders.Params paramsEncoder) (Decoders.Result decoder) preparable)

-- |
-- Execute a statement by providing parameters to it.
statement :: params -> Statement.Statement params result -> Session result
statement input (Statement.Statement template (Encoders.Params paramsEncoder) (Decoders.Result decoder) preparable) =
  Session
    $ ReaderT
    $ \(Connection.Connection usePreparedStatements pqConnectionRef integerDatetimes registry _) ->
      ExceptT
        $ fmap (first (QueryError template (Encoders.Params.renderReadable paramsEncoder input)))
        $ withMVar pqConnectionRef
        $ \pqConnection -> do
          r1 <- IO.sendParametricStatement pqConnection integerDatetimes registry template paramsEncoder (usePreparedStatements && preparable) input
          r2 <- IO.getResults pqConnection integerDatetimes decoder
          return $ r1 *> r2

-- |
-- Execute a pipeline.
pipeline :: Pipeline.Pipeline result -> Session result
pipeline pipeline =
  Session $ ReaderT \(Connection.Connection usePreparedStatements pqConnectionRef integerDatetimes registry _) ->
    ExceptT $ withMVar pqConnectionRef \pqConnection ->
      Pipeline.run pipeline usePreparedStatements pqConnection registry integerDatetimes

-- |
-- Look up an OID for a type name, using cache first, then querying the database.
lookupTypeOid :: Text -> Session PTI.OID
lookupTypeOid typeName = do
  connection <- ask
  cached <- liftIO $ Connection.lookupOidInCache connection typeName
  case cached of
    Just oid -> pure oid
    Nothing -> do
      oid <- queryTypeOid typeName
      liftIO $ Connection.addOidToCache connection typeName oid
      pure oid

-- |
-- Query the database for the OID of a given type name.
queryTypeOid :: Text -> Session PTI.OID
queryTypeOid typeName = do
  result <- statement typeName typeOidStatement
  case result of
    Just oidInt32 -> pure $ PTI.mkOID LibPQ.Binary (fromIntegral oidInt32)
    Nothing -> throwError $ QueryError "SELECT oid FROM pg_type WHERE typname = $1" [typeName] (ClientError (Just "Type not found"))
  where
    typeOidStatement = Statement.Statement
      "SELECT oid FROM pg_type WHERE typname = $1"
      (Encoders.param (Encoders.nonNullable Encoders.text))
      (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.int4)))
      True
