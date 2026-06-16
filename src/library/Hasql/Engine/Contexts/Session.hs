module Hasql.Engine.Contexts.Session where

import Hasql.Codecs.Encoders.Params qualified as Params
import Hasql.Comms.Roundtrip qualified as Comms.Roundtrip
import Hasql.Engine.Contexts.Pipeline qualified as Pipeline
import Hasql.Engine.Decoders.Result qualified as Decoders.Result
import Hasql.Engine.Errors qualified as Errors
import Hasql.Engine.Structures.ConnectionState qualified as ConnectionState
import Hasql.Platform.Prelude
import Pqi qualified

-- |
-- A sequence of operations to be executed in the context of a single database connection with exclusive access to it.
--
-- Construct sessions using helpers in this module such as
-- 'statement', 'pipeline' and 'script', or use 'onPqiConnection' for a low-level
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
--
-- Internally rank-2 over the connection type, keeping the public type parameter-free.
newtype Session a
  = Session
      ( forall c.
        (Pqi.IsConnection c) =>
        ConnectionState.ConnectionState c ->
        IO (Either Errors.SessionError a, ConnectionState.ConnectionState c)
      )

instance Functor Session where
  {-# INLINE fmap #-}
  fmap f (Session run) = Session \s -> do
    (res, s') <- run s
    pure (fmap f res, s')

instance Applicative Session where
  {-# INLINE pure #-}
  pure a = Session \s -> pure (Right a, s)
  {-# INLINE (<*>) #-}
  Session runF <*> Session runX = Session \s -> do
    (ef, s') <- runF s
    case ef of
      Left err -> pure (Left err, s')
      Right f -> do
        (ex, s'') <- runX s'
        pure (fmap f ex, s'')

instance Monad Session where
  {-# INLINE (>>=) #-}
  Session runX >>= f = Session \s -> do
    (ex, s') <- runX s
    case ex of
      Left err -> pure (Left err, s')
      Right x -> run (f x) s'

instance MonadError Errors.SessionError Session where
  {-# INLINE throwError #-}
  throwError err = Session \s -> pure (Left err, s)
  {-# INLINE catchError #-}
  catchError (Session runX) handler = Session \s -> do
    (ex, s') <- runX s
    case ex of
      Left err -> run (handler err) s'
      Right x -> pure (Right x, s')

instance MonadIO Session where
  {-# INLINE liftIO #-}
  liftIO action = Session \s -> do
    a <- action
    pure (Right a, s)

run :: (Pqi.IsConnection c) => Session a -> ConnectionState.ConnectionState c -> IO (Either Errors.SessionError a, ConnectionState.ConnectionState c)
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
pipeline pipelineAction = Session \connectionState -> do
  let usePreparedStatements = ConnectionState.preparedStatements connectionState
      statementCache = ConnectionState.statementCache connectionState
      oidCache = ConnectionState.oidCache connectionState
      pqConnection = ConnectionState.connection connectionState
   in do
        (result, newOidCache, newStatementCache) <- Pipeline.run pipelineAction usePreparedStatements pqConnection oidCache statementCache
        let newConnectionState =
              connectionState
                { ConnectionState.oidCache = newOidCache,
                  ConnectionState.statementCache = newStatementCache
                }

        pure (result, newConnectionState)

-- |
-- Execute an operation on the raw pqi connection possibly producing an error and updating the connection.
-- This is a low-level escape hatch for custom integrations.
--
-- You can supply a new connection in the result to replace it in the running Hasql connection.
-- The responsibility to close the old connection is on you.
-- Otherwise, just return the same connection you've received.
--
-- Producing a 'Left' value will cause the session to fail with the given error.
-- Regardless of success or failure, the connection will be replaced with the one you return.
--
-- Throwing exceptions is okay. It will lead to the connection getting reset.
onPqiConnection ::
  (forall conn. Pqi.IsConnection conn => conn -> IO (Either Errors.SessionError a, conn)) ->
  Session a
onPqiConnection f = Session \connectionState -> do
  let pqConnection = ConnectionState.connection connectionState
  (result, newConnection) <- f pqConnection
  let newState = ConnectionState.setConnection newConnection connectionState
  pure (result, newState)
