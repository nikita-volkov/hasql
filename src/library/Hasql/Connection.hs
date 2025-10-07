{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

-- |
-- This module provides a low-level effectful API dealing with the connections to the database.
module Hasql.Connection
  ( Connection,
    acquire,
    release,
    use,
  )
where

import Core.Contexts.Session qualified as Session
import Core.Errors
import Core.Structures.ConnectionState qualified as ConnectionState
import Core.Structures.StatementCache qualified as StatementCache
import Data.Text qualified as Text
import Hasql.Connection.Config qualified as Config
import Hasql.Connection.ServerVersion qualified as ServerVersion
import Hasql.Connection.Setting qualified as Setting
import Hasql.Errors qualified as Errors
import Platform.Prelude
import Pq qualified
import TextBuilder qualified

-- |
-- A single connection to the database.
newtype Connection
  = Connection (MVar ConnectionState.ConnectionState)

-- |
-- Establish a connection according to the provided settings.
acquire ::
  [Setting.Setting] ->
  IO (Either ConnectionError Connection)
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
      throwError (CompatibilityConnectionError ("Server version is lower than 10: " <> ServerVersion.toText version))

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
-- Blocks until the connection is available when there is another session running upon the connection.
use :: Connection -> Session.Session a -> IO (Either SessionError a)
use (Connection var) session =
  mask \restore -> do
    connectionState@ConnectionState.ConnectionState {..} <- takeMVar var
    result <- try @SomeException (restore (Session.run session connectionState))
    case result of
      Left exception -> do
        -- If an exception happened, we need to check the connection status.
        -- If the connection is not idle, we need to reset it
        -- and clear the prepared statement registry.
        Pq.transactionStatus connection >>= \case
          -- The connection is idle.
          Pq.TransIdle -> do
            -- If the connection is idle, just put back the connection state.
            putMVar var connectionState
            throwIO exception

          -- A command is in progress. This is unexpected. The connection is corrupted.
          -- Probably a leak in the masking of asynchronous exceptions.
          Pq.TransActive -> do
            putMVar var connectionState
            (pure . Left . Errors.DriverSessionError . TextBuilder.toText . mconcat)
              [ "Connection is in an unexpected state: a command is in progress. Report this as a bug. In the meantime, we've intercepted the following exception: ",
                TextBuilder.string (displayException exception)
              ]

          -- Idle, in a valid transaction block.
          Pq.TransInTrans -> do
            -- Abort the transaction.
            _ <- Pq.exec connection "ABORT;"
            putMVar var connectionState
            throwIO exception

          -- Idle, in a failed transaction block.
          Pq.TransInError -> do
            putMVar var connectionState
            throwIO exception

          -- The connection is bad.
          Pq.TransUnknown -> do
            putMVar var connectionState
            throwIO exception
      Right (result, !newState) -> do
        putMVar var newState
        pure result
