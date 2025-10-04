{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

-- |
-- This module provides a low-level effectful API dealing with the connections to the database.
module Hasql.Connection
  ( Connection,
    AcquisitionError (..),
    UsageError (..),
    acquire,
    release,
    use,
    withLibPQConnection,
  )
where

import Core.Contexts.Session qualified as Session
import Core.Structures.ConnectionState qualified as ConnectionState
import Core.Structures.StatementCache qualified as StatementCache
import Core.UsageError
import Data.Text qualified as Text
import Hasql.Connection.Config qualified as Config
import Hasql.Connection.ServerVersion qualified as ServerVersion
import Hasql.Connection.Setting qualified as Setting
import Platform.Prelude
import Pq qualified

-- |
-- A single connection to the database.
newtype Connection
  = Connection (MVar ConnectionState.ConnectionState)

-- |
-- Connection acquistion error.
data AcquisitionError
  = NetworkingAcquisitionError
  | AuthenticationAcquisitionError
  | CompatibilityAcquisitionError
      -- | Human readable details indended for logging.
      Text
  | -- | Uncategorized error coming from "libpq". May be empty text.
    OtherAcquisitionError
      -- | Human readable details intended for logging.
      Text
  deriving stock (Show, Eq)

-- |
-- Establish a connection according to the provided settings.
acquire ::
  [Setting.Setting] ->
  IO (Either AcquisitionError Connection)
acquire settings =
  {-# SCC "acquire" #-}
  runExceptT do
    pqConnection <- lift (Pq.connectdb (Config.connectionString config))

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
      throwError (CompatibilityAcquisitionError ("Server version is lower than 10: " <> ServerVersion.toText version))

    -- Initialize:
    lift do
      Pq.exec pqConnection do
        "SET client_encoding = 'UTF8';\n\
        \SET client_min_messages TO WARNING;"

    let connectionState =
          ConnectionState.ConnectionState
            { ConnectionState.preparedStatements = Config.usePreparedStatements config,
              ConnectionState.statementCache = StatementCache.empty,
              ConnectionState.connection = pqConnection
            }
    connectionRef <- lift (newMVar connectionState)
    pure (Connection connectionRef)
  where
    config = Config.fromUpdates settings

    interpretConnectionError :: Maybe ByteString -> AcquisitionError
    interpretConnectionError errorMessage =
      case errorMessage of
        Nothing -> OtherAcquisitionError "Unknown connection error"
        Just msg ->
          let msgLower = Text.toLower (decodeUtf8Lenient msg)
           in if
                | any (`Text.isInfixOf` msgLower) networkingErrors -> NetworkingAcquisitionError
                | any (`Text.isInfixOf` msgLower) authenticationErrors -> AuthenticationAcquisitionError
                | otherwise -> OtherAcquisitionError (decodeUtf8Lenient msg)

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
-- Execute an operation on the raw @libpq@ 'Pq.Connection'.
--
-- The access to the connection is exclusive.
{-# DEPRECATED withLibPQConnection "Use @Hasql.Session.'Hasql.Session.onLibpqConnection'@ instead" #-}
withLibPQConnection :: Connection -> (Pq.Connection -> IO a) -> IO a
withLibPQConnection connection action =
  useConnectionState connection \connectionState ->
    (,connectionState)
      <$> action (ConnectionState.connection connectionState)

-- |
-- Execute a sequence of operations with exclusive access to the connection.
--
-- Blocks until the connection is available when there is another session running upon the connection.
use :: Connection -> Session.Session a -> IO (Either UsageError a)
use connection session =
  useConnectionState connection \connectionState -> do
    Session.run session connectionState

useConnectionState :: Connection -> (ConnectionState.ConnectionState -> IO (a, ConnectionState.ConnectionState)) -> IO a
useConnectionState (Connection var) handler =
  mask \restore -> do
    connectionState@ConnectionState.ConnectionState {..} <- takeMVar var
    result <- try @SomeException (restore (handler connectionState))
    case result of
      Left exception -> do
        -- If an exception happened, we need to check the connection status.
        -- If the connection is not idle, we need to reset it
        -- and clear the prepared statement registry.
        Pq.transactionStatus connection >>= \case
          Pq.TransIdle -> do
            -- If the connection is idle, just put back the connection state.
            putMVar var connectionState
          _ -> do
            -- If the connection is not idle, reset the prepared statement registry
            -- and reset the connection itself.
            putMVar var (ConnectionState.resetPreparedStatementsCache connectionState)
            Pq.reset connection

        throwIO exception
      Right (result, !newState) -> do
        putMVar var newState
        pure result
