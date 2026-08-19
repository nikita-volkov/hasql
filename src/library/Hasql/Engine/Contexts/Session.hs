module Hasql.Engine.Contexts.Session where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Hasql.CodecsVocab.QualifiedTypeName qualified as CodecsVocab.QualifiedTypeName
import Hasql.Comms.Roundtrip qualified as Comms.Roundtrip
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
-- 'Errors.UseError' or the result:
--
-- > result <- Hasql.Connection.use connection mySession
--
-- The 'Control.Monad.Except.MonadError' instance ranges over
-- 'Errors.SessionError' only - the failures that leave the connection
-- usable. A failure that leaves the connection gone
-- ('Errors.ConnectionUseError', 'Errors.DriverUseError') is not a value
-- 'Control.Monad.Except.throwError' can construct or
-- 'Control.Monad.Except.catchError' can see: it propagates through every
-- later 'Control.Monad.>>=' untouched and reaches 'Hasql.Connection.use'
-- whole. This is enforced structurally by this 'Monad' instance rather than
-- by a check any handler has to remember to make.
--
-- A caught error retains every connection-state change made before it -
-- both branches below thread the state through, matching @ExceptT e (State
-- s)@ rather than @StateT s (Either e)@. A statement whose @PARSE@ succeeded
-- and whose @EXECUTE@ failed has truthfully been prepared on the server, so
-- the cache entry survives the catch; only 'statement' and 'pipeline'
-- revert it themselves, on an ordinary failure, since they alone know
-- whether what they recorded was actually confirmed by the server.
--
-- Note: while most session errors are returned as values, user code executed
-- inside a session may still throw exceptions, and an interruption from
-- another thread ('System.Timeout.timeout', 'Control.Concurrent.killThread')
-- arrives the same way. Such an exception propagates out of
-- 'Hasql.Connection.use', which finishes the connection on the way out rather
-- than trying to bring it back to a clean state.
newtype Session a
  = Session (ConnectionState.ConnectionState -> IO (Either Errors.UseError a, ConnectionState.ConnectionState))

run :: Session a -> ConnectionState.ConnectionState -> IO (Either Errors.UseError a, ConnectionState.ConnectionState)
run (Session session) connectionState = session connectionState

instance Functor Session where
  fmap f (Session m) = Session \connectionState -> do
    (result, newConnectionState) <- m connectionState
    pure (fmap f result, newConnectionState)

instance Applicative Session where
  pure a = Session \connectionState -> pure (Right a, connectionState)

  Session mf <*> Session ma = Session \connectionState -> do
    (resultF, connectionState') <- mf connectionState
    case resultF of
      Left err -> pure (Left err, connectionState')
      Right f -> do
        (resultA, connectionState'') <- ma connectionState'
        pure (fmap f resultA, connectionState'')

instance Monad Session where
  Session m >>= f = Session \connectionState -> do
    (result, connectionState') <- m connectionState
    case result of
      Left err -> pure (Left err, connectionState')
      Right a -> run (f a) connectionState'

instance MonadIO Session where
  liftIO io = Session \connectionState -> do
    a <- io
    pure (Right a, connectionState)

-- | Ranges over 'Errors.SessionError' only. 'throwError' can only construct
-- 'Errors.SessionUseError', and 'catchError' only ever sees that
-- constructor - 'Errors.ConnectionUseError' and 'Errors.DriverUseError'
-- flow past both untouched. See the note on the 'Session' type.
instance MonadError Errors.SessionError Session where
  throwError err = Session \connectionState -> pure (Left (Errors.SessionUseError err), connectionState)

  catchError (Session m) handler = Session \connectionState -> do
    (result, connectionState') <- m connectionState
    case result of
      Left (Errors.SessionUseError err) -> run (handler err) connectionState'
      other -> pure (other, connectionState')

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
        Comms.Roundtrip.ClientError _ connectionLost details -> do
          pure
            ( Left (Errors.fromSendError connectionLost details),
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
                    then Left (Errors.SessionUseError (Errors.MissingTypesSessionError (HashSet.map CodecsVocab.QualifiedTypeName.toNameTuple notFoundTypes)))
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
              Comms.Roundtrip.ClientError _ connectionLost details ->
                Errors.fromSendError connectionLost details
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
-- Throwing exceptions is okay, but it costs the connection:
-- 'Hasql.Connection.use' finishes the one it handed to the session and spends
-- the handle. It finishes that one - not any replacement you returned before
-- the exception, since an exception unwinds past the state carrying it. A
-- replacement lost that way is yours to close, like the connection it
-- displaced.
--
-- Restoring the connection is on you on the ordinary return path. Whatever
-- protocol state you leave it in - pipeline mode still on, results
-- undrained, a command in progress - is what the next session in the same
-- 'Hasql.Connection.use' inherits, and what gets handed back to the pool
-- afterwards. Nothing repairs it for you: the driver only ever finishes a
-- connection it cannot vouch for, and it has no way of telling that this
-- one is in that state. Returning a 'Left' of 'Errors.ConnectionUseError'
-- or 'Errors.DriverUseError' is how you say so - 'Hasql.Connection.use'
-- finishes the connection when it sees either.
onLibpqConnection ::
  (Pq.Connection -> IO (Either Errors.UseError a, Pq.Connection)) ->
  Session a
onLibpqConnection f = Session \connectionState -> do
  let pqConnection = ConnectionState.connection connectionState
  (result, newConnection) <- f pqConnection
  let newState = ConnectionState.setConnection newConnection connectionState
  pure (result, newState)
