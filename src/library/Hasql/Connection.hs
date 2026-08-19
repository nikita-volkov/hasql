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
  = Connection
      -- |
      -- The state is 'Nothing' once the connection is gone - either because
      -- 'release' was called on it, or because a session failed in a way that
      -- left the driver unable to vouch for the connection's protocol state, in
      -- which case 'use' finishes it (see 'Hasql.Engine.Errors.connectionIsSpent'). @libpq@ forbids
      -- touching a connection after @PQfinish@, so the handle has to remember
      -- that it did: without it a second 'release' finishes a freed connection
      -- and a later 'use' sends on one.
      (MVar (Maybe ConnectionState.ConnectionState))

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
            do
              execResult <-
                lift do
                  Pq.exec pqConnection do
                    "SET client_encoding = 'UTF8';\n\
                    \SET client_min_messages TO WARNING;"
              case execResult of
                Nothing -> do
                  errorMessage <- lift (Pq.errorMessage pqConnection)
                  throwError (OtherConnectionError (maybe "Failed to initialize the session" decodeUtf8Lenient errorMessage))
                Just result -> do
                  status <- lift (Pq.resultStatus result)
                  case status of
                    Pq.CommandOk -> pure ()
                    _ -> do
                      errorMessage <- lift (Pq.resultErrorMessage result)
                      throwError (OtherConnectionError (maybe "Failed to initialize the session" decodeUtf8Lenient errorMessage))

            let connectionState =
                  ConnectionState.ConnectionState
                    { ConnectionState.preparedStatements = not (Config.noPreparedStatements config),
                      ConnectionState.statementCache = StatementCache.empty,
                      ConnectionState.oidCache = mempty,
                      ConnectionState.connection = pqConnection,
                      ConnectionState.dead = False
                    }
            connectionRef <- lift (newMVar (Just connectionState))
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
    -- Best-effort classification by substring-matching libpq's error message.
    --
    -- libpq exposes no structured failure code for a failed @connectdb@ (no
    -- 'Pqi.Result' is ever produced to read a SQLSTATE off), so this is the
    -- only signal available. Two consequences follow from that:
    --
    -- - The match is against libpq's /translated/ message text, so it is
    --   sensitive to the client's locale (@LC_ALL@\/@LANG@\/@LANGUAGE@):
    --   under a non-English locale, none of the patterns below match, and
    --   the failure falls through to 'OtherConnectionError' regardless of
    --   its real nature.
    -- - Precedence is networking, then authentication, then everything
    --   else: a message matching both a networking and an authentication
    --   pattern is reported as networking.
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
        "connection refused",
        "timeout expired",
        "connection timed out",
        "host not found",
        "could not translate host name",
        "network is unreachable",
        "no route to host",
        -- Server-side rejections that are transient by nature: the server
        -- is there and reachable, it just isn't ready or able to serve this
        -- connection right now.
        "the database system is starting up",
        "the database system is in recovery mode",
        "sorry, too many clients already",
        "server closed the connection unexpectedly",
        "connection reset by peer",
        "could not fork new process",
        "terminating connection due to administrator command"
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
--
-- Idempotent: releasing a connection that is already gone - released
-- before, or finished by 'use' - does nothing.
release :: Connection -> IO ()
release (Connection var) =
  mask_ do
    connectionState <- takeMVar var
    traverse_ (Pq.finish . ConnectionState.connection) connectionState
    putMVar var Nothing

-- |
-- Execute a sequence of operations with exclusive access to the connection.
--
-- Blocks until the connection is available when there is another session running upon the connection on a different thread.
use :: Connection -> Session.Session a -> IO (Either SessionError a)
use (Connection var) session =
  mask \restore -> do
    takeMVar var >>= \case
      Nothing -> do
        putMVar var Nothing
        pure (Left (ConnectionSessionError "The connection has been released"))
      Just connectionState -> do
        let connection = ConnectionState.connection connectionState
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
                putMVar var Nothing
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
                putMVar var (Just (ConnectionState.resetPreparedStatementsCache connectionState))
                throwIO exception
          Right (result, !newState) ->
            if ConnectionState.dead newState
              then do
                -- The driver gave up on the connection somewhere inside the
                -- session. Whether the session went on to report that, to
                -- catch it and fail differently, or to catch it and
                -- succeed, the connection is not fit to be handed back.
                Pq.finish (ConnectionState.connection newState)
                putMVar var Nothing
                pure result
              else do
                putMVar var (Just newState)
                pure result
