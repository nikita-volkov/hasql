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
      -- The state is 'Nothing' once the connection is gone. That happens when
      -- 'release' is called on it, when a session fails in a way that leaves
      -- the driver unable to vouch for the connection's protocol state (a
      -- 'ConnectionUseError' out of 'use'), and when an exception cuts a
      -- session short - in the latter two cases 'use' finishes it. @libpq@
      -- forbids touching a connection after @PQfinish@, so the handle has to
      -- remember that it did: without it a second 'release' finishes a freed
      -- connection and a later 'use' sends on one.
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
-- On failure, returns a classified 'AcquireError' in 'Left'.
acquire ::
  Adapter ->
  Settings.Settings ->
  IO (Either AcquireError Connection)
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
              throwError (CompatibilityAcquireError ("Server version is lower than 9: " <> ServerVersion.toText version))

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
                  throwError (OtherAcquireError (maybe "Failed to initialize the session" decodeUtf8Lenient errorMessage))
                Just result -> do
                  status <- lift (Pq.resultStatus result)
                  case status of
                    Pq.CommandOk -> pure ()
                    _ -> do
                      errorMessage <- lift (Pq.resultErrorMessage result)
                      throwError (OtherAcquireError (maybe "Failed to initialize the session" decodeUtf8Lenient errorMessage))

            let connectionState =
                  ConnectionState.ConnectionState
                    { ConnectionState.preparedStatements = not (Config.noPreparedStatements config),
                      ConnectionState.statementCache = StatementCache.empty,
                      ConnectionState.oidCache = mempty,
                      ConnectionState.connection = pqConnection
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
    --   the failure falls through to 'OtherAcquireError' regardless of
    --   its real nature.
    -- - Precedence is networking, then authentication, then everything
    --   else: a message matching both a networking and an authentication
    --   pattern is reported as networking.
    interpretConnectionError :: Maybe ByteString -> AcquireError
    interpretConnectionError errorMessage =
      case errorMessage of
        Nothing -> OtherAcquireError "Unknown connection error"
        Just msg ->
          let msgText = decodeUtf8Lenient msg
              msgLower = Text.toLower msgText
           in if
                | any (`Text.isInfixOf` msgLower) networkingErrors -> NetworkingAcquireError msgText
                | any (`Text.isInfixOf` msgLower) authenticationErrors -> AuthenticationAcquireError msgText
                | otherwise -> OtherAcquireError (decodeUtf8Lenient msg)

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
    -- The handle is marked spent before the connection is touched, so that a
    -- 'Pq.finish' that somehow fails leaves the handle unusable rather than
    -- the MVar empty forever.
    putMVar var Nothing
    traverse_ (Pq.finish . ConnectionState.connection) connectionState

-- |
-- Execute a sequence of operations with exclusive access to the connection.
--
-- Blocks until the connection is available when there is another session running upon the connection on a different thread.
--
-- An exception thrown out of the session - including an interruption
-- delivered from another thread, as 'System.Timeout.timeout' and
-- 'Control.Concurrent.killThread' do - propagates, and the connection is
-- finished on the way out. See the note on the exception path below for why
-- it is not repaired instead.
use :: Connection -> Session.Session a -> IO (Either UseError a)
use (Connection var) session =
  mask \unmask -> do
    takeMVar var >>= \case
      Nothing -> do
        putMVar var Nothing
        pure (Left (ConnectionUseError "The connection is no longer available"))
      Just connectionState -> do
        (result, !newState) <-
          onException (unmask (Session.run session connectionState)) do
            -- An exception leaves the connection somewhere inside a round
            -- trip, and the driver has no way of finding out where. Repairing
            -- it means draining results and aborting the transaction -
            -- blocking network IO, performed here under a mask, which on a
            -- connection whose peer has gone away never returns. The
            -- interruption being handled would then never land, which is the
            -- opposite of what the caller asked for. Finishing the connection
            -- touches nothing but the socket.
            --
            -- The connection finished here is the one taken out of the MVar. A
            -- session that replaced it through 'Hasql.Session.onLibpqConnection'
            -- loses the replacement along with the rest of the state the
            -- exception unwound past; closing that one is on the caller, which
            -- is what the escape hatch already promises.
            putMVar var Nothing
            Pq.finish (ConnectionState.connection connectionState)

        -- The driver gave up on the connection somewhere inside the session,
        -- and by construction that verdict is what 'result' says: nothing
        -- further inside the session could have caught it and carried on.
        let finish = do
              putMVar var Nothing
              Pq.finish (ConnectionState.connection newState)
        case result of
          Left (ConnectionUseError _) -> finish
          _ -> putMVar var (Just newState)

        pure result
