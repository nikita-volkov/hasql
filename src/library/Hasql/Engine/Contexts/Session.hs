module Hasql.Engine.Contexts.Session where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Hasql.CodecsVocab.QualifiedTypeName qualified as CodecsVocab.QualifiedTypeName
import Hasql.Comms.Roundtrip qualified as Comms.Roundtrip
import Hasql.Comms.Send qualified as Comms.Send
import Hasql.ConnectionState qualified as ConnectionState
import Hasql.ConnectionState.OidCache qualified as OidCache
import Hasql.ConnectionState.StatementCache qualified as StatementCache
import Hasql.Engine.Contexts.Pipeline qualified as Pipeline
import Hasql.Engine.Errors qualified as Errors
import Hasql.Engine.PqProcedures.SelectTypeInfo qualified as PqProcedures.SelectTypeInfo
import Hasql.Engine.Statement qualified as Statement
import Hasql.Platform.Prelude
import Pqi qualified as Pq

-- |
-- A sequence of operations to be executed in the context of a single database connection with exclusive access to it.
--
-- Construct sessions using helpers in this module such as
-- 'statement', 'pipeline' and 'script', or use 'onLibpqConnection' for a low-level
-- escape hatch.
--
-- To actually execute a 'Session' use 'Hasql.Connection.use', which manages
-- concurrent access to the shared connection state and returns either a
-- 'Errors.SessionError' or the result:
--
-- > result <- Hasql.Connection.use connection mySession
--
-- Note: while most session errors are returned as values, user code executed
-- inside a session may still throw exceptions; in that case the driver will
-- reset the connection to a clean state.
newtype Session a
  = Session (ConnectionState.ConnectionState -> IO (Either Errors.SessionError a, ConnectionState.ConnectionState))
  deriving
    (Functor, Applicative, Monad, MonadError Errors.SessionError, MonadIO)
    via (ExceptT Errors.SessionError (StateT ConnectionState.ConnectionState IO))

run :: Session a -> ConnectionState.ConnectionState -> IO (Either Errors.SessionError a, ConnectionState.ConnectionState)
run (Session session) connectionState = session connectionState

-- |
-- Possibly a multi-statement query,
-- which however cannot be parameterized or prepared,
-- nor can any results of it be collected.
script :: ByteString -> Session ()
script sql =
  Session \connectionState -> do
    let connection = ConnectionState.connection connectionState
    result <- Comms.Roundtrip.toSerialIO (Comms.Roundtrip.script (Just sql) sql) connection
    case result of
      Left err -> case err of
        Comms.Roundtrip.ClientError _ cause details -> do
          pure
            ( Left
                ( case cause of
                    Comms.Send.ConnectionLoss -> Errors.ConnectionSessionError details
                    Comms.Send.ClientRejection -> Errors.ClientRejectionSessionError details
                ),
              connectionState
            )
        Comms.Roundtrip.ServerError recvError ->
          pure
            ( Left (Errors.fromRecvErrorInScript sql recvError),
              connectionState
            )
      Right () ->
        pure
          ( Right (),
            connectionState
          )

-- |
-- Execute a single statement by providing parameters to it,
-- running it directly in serial mode.
--
-- Each execution is a dedicated network roundtrip. The first execution of a
-- preparable statement costs an extra roundtrip (a separate @PARSE@), after
-- which steady-state execution is a single roundtrip.
--
-- To batch multiple statements into fewer roundtrips, use 'pipeline' instead.
statement ::
  Statement.Statement params result ->
  params ->
  Session result
statement stmt params =
  Session \connectionState -> do
    let usePreparedStatements = ConnectionState.preparedStatements connectionState
        statementCache = ConnectionState.statementCache connectionState
        oidCache = ConnectionState.oidCache connectionState
        connection = ConnectionState.connection connectionState
        sql = Statement.sql stmt
        missingTypes = OidCache.selectUnknownNames (Statement.unknownTypes stmt) oidCache
    resolvedOidCache <-
      if HashSet.null missingTypes
        then pure (Right oidCache)
        else do
          oidCacheUpdates <-
            PqProcedures.SelectTypeInfo.run connection (PqProcedures.SelectTypeInfo.SelectTypeInfo missingTypes)
          pure $ case oidCacheUpdates of
            Left err -> Left err
            Right oidCacheUpdates ->
              let foundTypes = HashMap.keysSet oidCacheUpdates
                  notFoundTypes = HashSet.difference missingTypes foundTypes
               in if not (HashSet.null notFoundTypes)
                    then Left (Errors.MissingTypesSessionError (HashSet.map CodecsVocab.QualifiedTypeName.toNameTuple notFoundTypes))
                    else Right (oidCache <> OidCache.fromHashMap oidCacheUpdates)
    case resolvedOidCache of
      Left err -> pure (Left err, connectionState)
      Right newOidCache -> do
        let resolve = OidCache.toResolver newOidCache
            decoder' = Statement.decoder stmt resolve
            prepared = usePreparedStatements && Statement.isPrepared stmt
            -- Single-statement tag for error reporting:
            -- total statements 1, index 0.
            tag = Just (1, 0, sql, Statement.printer stmt params, prepared)
            mapError = \case
              Comms.Roundtrip.ClientError _ cause details ->
                case cause of
                  Comms.Send.ConnectionLoss -> Errors.ConnectionSessionError details
                  Comms.Send.ClientRejection -> Errors.ClientRejectionSessionError details
              Comms.Roundtrip.ServerError recvError ->
                Errors.fromRecvError recvError
            withState (result, newStatementCache) =
              ( first mapError result,
                connectionState
                  { ConnectionState.oidCache = newOidCache,
                    ConnectionState.statementCache = newStatementCache
                  }
              )
        fmap withState
          $ if prepared
            then do
              let (oidList, valueAndFormatList) =
                    Statement.compilePreparedStatementData stmt resolve params
                  encodedParams =
                    valueAndFormatList
                      & fmap (fmap (\(bytes, format) -> (bytes, bool Pq.Binary Pq.Text format)))
                  execute remoteKey =
                    Comms.Roundtrip.toSerialIO
                      (Comms.Roundtrip.queryPrepared tag remoteKey encodedParams Pq.Binary decoder')
                      connection
              case StatementCache.lookup sql oidList statementCache of
                Just remoteKey -> do
                  result <- execute remoteKey
                  pure (result, statementCache)
                Nothing -> do
                  let (remoteKey, newStatementCache) = StatementCache.insert sql oidList statementCache
                  -- In non-pipeline mode PARSE and EXECUTE cannot be sent
                  -- back-to-back, so prepare in a dedicated roundtrip first.
                  prepareResult <-
                    Comms.Roundtrip.toSerialIO
                      (Comms.Roundtrip.prepare tag remoteKey sql oidList)
                      connection
                  case prepareResult of
                    -- PARSE failed. Ordinarily the statement is not on the
                    -- server, so the old cache is kept (no entry committed).
                    -- The one exception is 42P05 ("prepared statement
                    -- already exists"): since names are content-addressed,
                    -- a collision on the name is a collision on the
                    -- statement, so whatever the server holds under
                    -- remoteKey already is this statement. Commit the cache
                    -- so the next use finds it warm instead of failing PARSE
                    -- again.
                    Left err ->
                      pure
                        ( Left err,
                          if Errors.isPrepareCollision err
                            then newStatementCache
                            else statementCache
                        )
                    Right () -> do
                      -- PARSE succeeded, so the statement is on the server
                      -- under remoteKey regardless of whether EXECUTE then
                      -- fails. Commit the cache so a later use hits it instead
                      -- of re-issuing PARSE for an already-existing name.
                      result <- execute remoteKey
                      pure (result, newStatementCache)
            else do
              let encodedParams =
                    Statement.compileUnpreparedStatementData stmt resolve params
                      & fmap (fmap (\(oid, bytes, format) -> (oid, bytes, bool Pq.Binary Pq.Text format)))
              result <-
                Comms.Roundtrip.toSerialIO
                  (Comms.Roundtrip.queryParams tag sql encodedParams Pq.Binary decoder')
                  connection
              pure (result, statementCache)

-- |
-- Execute a pipeline.
pipeline :: Pipeline.Pipeline result -> Session result
pipeline pipeline = Session (Pipeline.run pipeline)

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
onLibpqConnection ::
  (Pq.Connection -> IO (Either Errors.SessionError a, Pq.Connection)) ->
  Session a
onLibpqConnection f = Session \connectionState -> do
  let pqConnection = ConnectionState.connection connectionState
  (result, newConnection) <- f pqConnection
  let newState = ConnectionState.setConnection newConnection connectionState
  pure (result, newState)
