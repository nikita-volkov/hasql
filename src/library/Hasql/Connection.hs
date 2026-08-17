-- |
-- This module provides a low-level effectful API dealing with the connections to the database.
module Hasql.Connection
  ( Connection,
    acquire,
    release,
    use,
  )
where

import Data.Text qualified as Text
import Hasql.Comms.Session qualified as Comms.Session
import Hasql.Connection.Config qualified as Config
import Hasql.Connection.ServerVersion qualified as ServerVersion
import Hasql.Connection.Settings qualified as Settings
import Hasql.ConnectionState qualified as ConnectionState
import Hasql.ConnectionState.StatementCache qualified as StatementCache
import Hasql.Engine.Contexts.Session qualified as Session
import Hasql.Engine.Errors
import Hasql.Platform.Prelude
import Pqi (Adapter)
import Pqi qualified as Pq

-- |
-- A single connection to the database.
newtype Connection
  = Connection (MVar ConnectionState.ConnectionState)

-- |
-- Establish a connection according to the provided settings.
--
-- The first argument is an 'Pqi.Adapter', which defines the backend
-- implementation used to talk to PostgreSQL (for example, libpq via the
-- <https://hackage.haskell.org/package/pqi-ffi pqi-ffi> package, or a pure
-- Haskell implementation via the
-- <https://hackage.haskell.org/package/pqi-native pqi-native> package).
-- This is the only place in the library where users choose the adapter.
--
-- This function:
--
-- - Opens a PostgreSQL connection using the constructed connection string.
-- - Validates that the connection is healthy.
-- - Checks the server version for compatibility.
-- - Initializes session-level settings (encoding and message verbosity).
--
-- On success, returns a 'Connection' wrapped in 'Right'.
-- On failure, returns a classified 'ConnectionError' in 'Left'.
acquire ::
  Adapter ->
  Settings.Settings ->
  IO (Either ConnectionError Connection)
acquire adapter settings =
  {-# SCC "acquire" #-}
  runExceptT do
    let config = Config.construct settings

    -- Connect:
    ExceptT do
      bracketOnError
        (Pq.connectdb adapter (Config.connectionString config))
        Pq.finish
        \pqConnection -> do
          result <- runExceptT do
            -- Check status:
            status <- lift (Pq.status pqConnection)
            case status of
              Pq.ConnectionOk -> pure ()
              _ -> do
                errorMessage <- lift (Pq.errorMessage pqConnection)
                throwError (interpretConnectionError errorMessage)

            -- Check version:
            version <- lift (ServerVersion.load pqConnection)
            when (version < ServerVersion.minimum) do
              throwError (CompatibilityConnectionError ("Server version is lower than 9: " <> ServerVersion.toText version))

            -- Initialize:
            lift do
              Pq.exec pqConnection do
                "SET client_encoding = 'UTF8';\n\
                \SET client_min_messages TO WARNING;"

            let connectionState =
                  ConnectionState.ConnectionState
                    { ConnectionState.preparedStatements = not (Config.noPreparedStatements config),
                      ConnectionState.statementCache = StatementCache.empty,
                      ConnectionState.oidCache = mempty,
                      ConnectionState.connection = pqConnection
                    }
            connectionRef <- lift (newMVar connectionState)
            pure (Connection connectionRef)

          -- A classified failure (as opposed to a thrown exception, which
          -- 'bracketOnError' already finishes the connection for) still
          -- needs the connection finished, since it doesn't escape this
          -- action as an exception.
          case result of
            Left _ -> Pq.finish pqConnection
            Right _ -> pure ()
          pure result
  where
    interpretConnectionError :: Maybe ByteString -> ConnectionError
    interpretConnectionError errorMessage =
      case errorMessage of
        Nothing -> OtherConnectionError "Unknown connection error"
        Just msg ->
          let msgText = decodeUtf8Lenient msg
              msgLower = Text.toLower msgText
           in if
                | any (`Text.isInfixOf` msgLower) networkingErrors -> NetworkingConnectionError msgText
                | any (`Text.isInfixOf` msgLower) authenticationErrors -> AuthenticationConnectionError msgText
                | otherwise -> OtherConnectionError (decodeUtf8Lenient msg)

    networkingErrors :: [Text]
    networkingErrors =
      [ "could not connect to server",
        "no such file or directory",
        "connection refused",
        "timeout expired",
        "host not found",
        "could not translate host name"
      ]

    authenticationErrors :: [Text]
    authenticationErrors =
      [ "authentication failed",
        "password authentication failed",
        "no password supplied",
        "peer authentication failed"
      ]

-- |
-- Release the connection.
release :: Connection -> IO ()
release (Connection connectionRef) =
  mask_ do
    connectionState <- readMVar connectionRef
    Pq.finish (ConnectionState.connection connectionState)

-- |
-- Execute a sequence of operations with exclusive access to the connection.
--
-- Blocks until the connection is available when there is another session running upon the connection on a different thread.
use :: Connection -> Session.Session a -> IO (Either SessionError a)
use (Connection var) session =
  mask \restore -> do
    connectionState@ConnectionState.ConnectionState {..} <- takeMVar var
    result <- try @SomeException (restore (Session.run session connectionState))
    case result of
      Left exception -> do
        -- If an exception happened, we need to bring the connection back to idle
        -- without resetting (to preserve session state).
        result <- Comms.Session.toHandler Comms.Session.cleanUpAfterInterruption connection
        case result of
          Left err -> do
            -- If cleanup failed, we have to close the connection.
            -- There's not much else we can do.
            Pq.finish connection
            putMVar var (ConnectionState.resetPreparedStatementsCache connectionState)
            let message =
                  mconcat
                    [ "Failed to clean up after interruption.\n",
                      err,
                      "\n",
                      "The following exception was raised during the operation:\n",
                      Text.pack (displayException exception)
                    ]
            pure (Left (DriverSessionError message))
          Right () -> do
            putMVar var (ConnectionState.resetPreparedStatementsCache connectionState)
            throwIO exception
      Right (result, !newState) -> do
        putMVar var newState
        pure result
