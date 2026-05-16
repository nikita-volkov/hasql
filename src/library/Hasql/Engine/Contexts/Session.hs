module Hasql.Engine.Contexts.Session where

import Hasql.Codecs.Encoders.Params qualified as Params
import Hasql.Comms.Roundtrip qualified as Comms.Roundtrip
import Hasql.Engine.Contexts.Pipeline qualified as Pipeline
import Hasql.Engine.Decoders.Result qualified as Decoders.Result
import Hasql.Engine.Errors qualified as Errors
import Hasql.Engine.Structures.ConnectionState qualified as ConnectionState
import Hasql.Platform.Prelude

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
newtype Session conn a
  = Session
      ( forall result.
        ConnectionState.ConnectionState conn result ->
        IO (Either Errors.SessionError a, ConnectionState.ConnectionState conn result)
      )

-- * Instances

instance Functor (Session conn) where
  {-# INLINE fmap #-}
  fmap f (Session g) = Session \cs -> do
    (ea, cs') <- g cs
    pure (fmap f ea, cs')

instance Applicative (Session conn) where
  {-# INLINE pure #-}
  pure a = Session \cs -> pure (Right a, cs)
  {-# INLINE (<*>) #-}
  Session gf <*> Session ga = Session \cs -> do
    (ef, cs') <- gf cs
    case ef of
      Left err -> pure (Left err, cs')
      Right f -> do
        (ea, cs'') <- ga cs'
        pure (fmap f ea, cs'')

instance Monad (Session conn) where
  {-# INLINE (>>=) #-}
  Session ga >>= f = Session \cs -> do
    (ea, cs') <- ga cs
    case ea of
      Left err -> pure (Left err, cs')
      Right a -> let Session gb = f a in gb cs'

instance MonadError Errors.SessionError (Session conn) where
  {-# INLINE throwError #-}
  throwError e = Session \cs -> pure (Left e, cs)
  {-# INLINE catchError #-}
  catchError (Session g) h = Session \cs -> do
    (ea, cs') <- g cs
    case ea of
      Left err -> let Session gb = h err in gb cs'
      Right a -> pure (Right a, cs')

instance MonadIO (Session conn) where
  {-# INLINE liftIO #-}
  liftIO action = Session \cs -> do
    a <- action
    pure (Right a, cs)

run :: Session conn a -> ConnectionState.ConnectionState conn result -> IO (Either Errors.SessionError a, ConnectionState.ConnectionState conn result)
run (Session session) connectionState = session connectionState

-- |
-- Possibly a multi-statement query,
-- which however cannot be parameterized or prepared,
-- nor can any results of it be collected.
script :: ByteString -> Session conn ()
script sql =
  Session \connectionState -> do
    let drv = ConnectionState.driver connectionState
        connection = ConnectionState.connection connectionState
    result <- Comms.Roundtrip.toSerialIO (Comms.Roundtrip.script drv (Just sql) sql) connection
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
  Session conn result
statement sql paramsEncoder decoder preparable params =
  pipeline (Pipeline.statement sql paramsEncoder decoder preparable params)

-- |
-- Execute a pipeline.
pipeline :: Pipeline.Pipeline conn result -> Session conn result
pipeline pipelineAction = Session \connectionState -> do
  let drv = ConnectionState.driver connectionState
      usePreparedStatements = ConnectionState.preparedStatements connectionState
      statementCache = ConnectionState.statementCache connectionState
      oidCache = ConnectionState.oidCache connectionState
      pqConnection = ConnectionState.connection connectionState
   in do
        (result, newOidCache, newStatementCache) <- Pipeline.run drv pipelineAction usePreparedStatements pqConnection oidCache statementCache
        let newConnectionState =
              connectionState
                { ConnectionState.oidCache = newOidCache,
                  ConnectionState.statementCache = newStatementCache
                }

        pure (result, newConnectionState)

-- |
-- Execute an operation on the raw connection possibly producing an error and updating the connection.
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
onConnection ::
  (conn -> IO (Either Errors.SessionError a, conn)) ->
  Session conn a
onConnection f = Session \connectionState -> do
  let pqConnection = ConnectionState.connection connectionState
  (result, newConnection) <- f pqConnection
  let newState = ConnectionState.setConnection newConnection connectionState
  pure (result, newState)
