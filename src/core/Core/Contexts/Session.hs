module Core.Contexts.Session where

import Codecs.Encoders.Params qualified as Params
import Comms.Roundtrip qualified
import Core.Contexts.Pipeline qualified as Pipeline
import Core.Decoders.Result qualified as Decoders.Result
import Core.Errors qualified as Errors
import Core.Structures.ConnectionState qualified as ConnectionState
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
    result <- Comms.Roundtrip.toSerialIO (Comms.Roundtrip.query (Just sql) sql) connection
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
-- Execute a statement by providing parameters to it.
statement ::
  ByteString ->
  Params.Params params ->
  Decoders.Result.Result result ->
  Bool ->
  params ->
  Session result
statement sql paramsEncoder decoder preparable params =
  pipeline (Pipeline.statement sql paramsEncoder decoder preparable params)

-- |
-- Execute a pipeline.
pipeline :: Pipeline.Pipeline result -> Session result
pipeline pipeline = Session \connectionState -> do
  let usePreparedStatements = ConnectionState.preparedStatements connectionState
      statementCache = ConnectionState.statementCache connectionState
      oidCache = ConnectionState.oidCache connectionState
      pqConnection = ConnectionState.connection connectionState
   in do
        (result, newOidCache, newStatementCache) <- Pipeline.run pipeline usePreparedStatements pqConnection oidCache statementCache
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
