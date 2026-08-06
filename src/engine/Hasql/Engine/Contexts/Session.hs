module Hasql.Engine.Contexts.Session where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Hasql.Codecs.RequestingOid qualified as RequestingOid
import Hasql.Codecs.Vocab.OidCache qualified as OidCache
import Hasql.Codecs.Vocab.QualifiedTypeName qualified as Vocab.QualifiedTypeName
import Hasql.Comms.Roundtrip qualified as Comms.Roundtrip
import Hasql.Engine.Contexts.Pipeline qualified as Pipeline
import Hasql.Engine.Errors qualified as Errors
import Hasql.Engine.PqProcedures.SelectTypeInfo qualified as PqProcedures.SelectTypeInfo
import Hasql.Engine.Statement qualified as Statement
import Hasql.Engine.Structures.ConnectionState qualified as ConnectionState
import Hasql.Engine.Structures.ExecutionContext (ExecutionContext (ExecutionContext))
import Hasql.Engine.Structures.ExecutionContext qualified as ExecutionContext
import Hasql.Engine.Structures.StatementCache qualified as StatementCache
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
        Comms.Roundtrip.ClientError _ details -> do
          pure
            ( Left (Errors.ConnectionSessionError (maybe "" decodeUtf8Lenient details)),
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
-- Statistics of the connection's prepared statement cache.
--
-- Exposed as a session action rather than an operation on the connection so
-- that it reads the state while the connection is already held, instead of
-- blocking behind a running session.
statementCacheStats :: Session StatementCache.StatementCacheStats
statementCacheStats =
  Session \connectionState ->
    pure
      ( Right (StatementCache.toStats (ConnectionState.statementCache connectionState)),
        connectionState
      )

-- |
-- Restore exact agreement with the server about which statements are prepared,
-- when a client-side failure left that in doubt.
--
-- A no-op unless the cache was marked desynced, in which case it costs one
-- roundtrip on a connection that is already in trouble.
resyncStatementCache :: Session ()
resyncStatementCache =
  Session \connectionState ->
    if StatementCache.isDesynced (ConnectionState.statementCache connectionState)
      then do
        result <-
          Comms.Roundtrip.toSerialIO
            (Comms.Roundtrip.query () "DEALLOCATE ALL")
            (ConnectionState.connection connectionState)
        pure case result of
          Left err -> (Left (Errors.fromRoundtripError err), connectionState)
          -- Only now, with the server-side set demonstrably empty, is it safe
          -- to reuse remote key names.
          Right () -> (Right (), ConnectionState.resetPreparedStatementsCache connectionState)
      else pure (Right (), connectionState)

-- |
-- Execute a single statement by providing parameters to it,
-- running it directly in serial mode.
--
-- Each execution is a dedicated network roundtrip. The execution that first
-- gets the statement prepared additionally carries the @PARSE@ and the
-- deallocation of whatever it displaced, all in that same roundtrip.
--
-- To batch multiple statements into fewer roundtrips, use 'pipeline' instead.
statement ::
  Statement.Statement params result ->
  params ->
  Session result
statement stmt params =
  Session \connectionState -> do
    let statementCache = ConnectionState.statementCache connectionState
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
                    then Left (Errors.MissingTypesSessionError (HashSet.map Vocab.QualifiedTypeName.toNameTuple notFoundTypes))
                    else Right (oidCache <> OidCache.fromHashMap oidCacheUpdates)
    case resolvedOidCache of
      Left err -> pure (Left err, connectionState)
      Right newOidCache -> do
        let decoder = RequestingOid.toBase (Statement.decoder stmt) newOidCache

            (oidList, encodedParams) = Statement.compileStatementData stmt newOidCache params

            pqOidList = oidList

            pqEncodedParams =
              fmap (fmap (second (bool Pq.Binary Pq.Text))) encodedParams

            localKey = StatementCache.localKey sql pqOidList

            (decision, decidedCache) = StatementCache.decide localKey statementCache

            -- Executed alone, so the statement is the only one of one.
            context isPrepared localKey statementCache =
              Just
                ExecutionContext
                  { statementIndex = 0,
                    sql = sql,
                    params = Statement.printer stmt params,
                    isPrepared = isPrepared,
                    localKey = localKey,
                    statementCache = statementCache
                  }

            executePrepared cache remoteKey =
              Comms.Roundtrip.queryPrepared
                (context True (Just localKey) cache)
                remoteKey
                pqEncodedParams
                Pq.Binary
                decoder

            executeUnprepared cache =
              Comms.Roundtrip.queryParams
                (context False Nothing cache)
                sql
                (Statement.inlineOids pqOidList pqEncodedParams)
                Pq.Binary
                decoder

            mapError = \case
              Comms.Roundtrip.ClientError _ details ->
                Errors.ConnectionSessionError (maybe "" decodeUtf8Lenient details)
              Comms.Roundtrip.ServerError recvError ->
                Errors.fromRecvError (fmap (fmap (ExecutionContext.toStatementLocation 1)) recvError)

            withState (result, newStatementCache) =
              ( first mapError result,
                connectionState
                  { ConnectionState.oidCache = newOidCache,
                    ConnectionState.statementCache = newStatementCache
                  }
              )

            -- Both conditions we recover from are raised before the statement
            -- does anything, so retrying is semantically safe. The retry goes
            -- through the unprepared path, which is structurally immune to a
            -- stale cached plan and spends no PARSE on a statement whose plan
            -- just proved unstable. Inside an open transaction block, though,
            -- any error has already poisoned it, so a retry is guaranteed to
            -- fail with 25P02.
            recover isPipelined okCache failure = do
              let repaired = ExecutionContext.recoverStatementCache isPipelined okCache failure
              case Errors.toSqlState failure of
                Just sqlState | sqlState == "0A000" || sqlState == "26000" -> do
                  transactionStatus <- Pq.transactionStatus connection
                  if transactionStatus == Pq.TransIdle
                    then do
                      retryResult <- Comms.Roundtrip.toSerialIO (executeUnprepared repaired) connection
                      pure (retryResult, repaired)
                    else pure (Left failure, repaired)
                _ -> pure (Left failure, repaired)

        fmap withState case decision of
          StatementCache.HitDecision remoteKey -> do
            result <- Comms.Roundtrip.toSerialIO (executePrepared decidedCache remoteKey) connection
            case result of
              Right _ -> pure (result, decidedCache)
              Left failure -> recover False decidedCache failure
          StatementCache.MissDecision -> do
            -- Nothing here depends on the cache, so nothing here can be
            -- invalidated by it.
            result <- Comms.Roundtrip.toSerialIO (executeUnprepared decidedCache) connection
            pure (result, decidedCache)
          StatementCache.AdmitDecision remoteKey victim -> do
            -- Pipeline mode, because libpq forbids sending PARSE and EXECUTE
            -- back to back outside of it. That makes the admission cost one
            -- roundtrip where a serial first-prepare would cost two.
            let roundtrip =
                  maybe
                    (pure ())
                    (Comms.Roundtrip.deallocate (context True (Just localKey) statementCache))
                    victim
                    *> Comms.Roundtrip.prepare
                      (context True (Just localKey) (StatementCache.withdraw localKey decidedCache))
                      remoteKey
                      sql
                      pqOidList
                    *> executePrepared decidedCache remoteKey
            result <- Comms.Roundtrip.toPipelineIO roundtrip Nothing connection
            case result of
              Right _ -> pure (result, decidedCache)
              Left failure -> recover True decidedCache failure

-- |
-- Execute a pipeline.
pipeline :: Pipeline.Pipeline result -> Session result
pipeline pipeline = Session \connectionState -> do
  let statementCache = ConnectionState.statementCache connectionState
      oidCache = ConnectionState.oidCache connectionState
      pqConnection = ConnectionState.connection connectionState
   in do
        (result, newOidCache, newStatementCache) <- Pipeline.run pipeline pqConnection oidCache statementCache
        let newConnectionState =
              connectionState
                { ConnectionState.oidCache = newOidCache,
                  ConnectionState.statementCache = newStatementCache
                }

        pure (result, newConnectionState)

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
