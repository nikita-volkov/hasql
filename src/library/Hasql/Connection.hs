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
import Platform.Prelude
import Pq qualified

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
        -- If an exception happened, we need to bring the connection back to idle
        -- without resetting (to preserve session state).
        
        -- First, check if we're in pipeline mode and try to exit it.
        pipelineStatus <- Pq.pipelineStatus connection
        when (pipelineStatus == Pq.PipelineOn) do
          -- Try to exit pipeline mode first.
          success <- Pq.exitPipelineMode connection
          -- If exit failed, it might be due to pending results. Drain them and try again.
          unless success do
            drainResults connection
            _ <- Pq.exitPipelineMode connection
            pure ()
        
        -- Now handle the transaction status to ensure we're back to idle.
        Pq.transactionStatus connection >>= \case
          Pq.TransIdle -> do
            -- Connection is already idle, just put back the connection state.
            putMVar var connectionState
          Pq.TransInTrans -> do
            -- In a transaction block: abort the transaction to return to idle.
            -- This preserves session-level state while cleaning up the transaction.
            _ <- Pq.exec connection "ABORT"
            putMVar var connectionState
          Pq.TransActive -> do
            -- A command is still in progress.
            -- Cancel it and drain results.
            mCancel <- Pq.getCancel connection
            case mCancel of
              Just cancel -> do
                _ <- Pq.cancel cancel
                pure ()
              Nothing -> pure ()
            -- Consume any pending data.
            _ <- Pq.consumeInput connection
            -- Drain all pending results.
            drainResults connection
            -- After draining, check status again and handle accordingly.
            Pq.transactionStatus connection >>= \case
              Pq.TransIdle -> putMVar var connectionState
              Pq.TransInTrans -> do
                _ <- Pq.exec connection "ABORT"
                putMVar var connectionState
              Pq.TransInError -> do
                _ <- Pq.exec connection "ABORT"
                putMVar var connectionState
              _ -> do
                -- For other states, reset as fallback but this shouldn't happen.
                Pq.reset connection
                putMVar var (ConnectionState.resetPreparedStatementsCache connectionState)
          Pq.TransInError -> do
            -- Transaction is in error state: abort to return to idle.
            _ <- Pq.exec connection "ABORT"
            putMVar var connectionState
          Pq.TransUnknown -> do
            -- Unknown state (connection issue): reset the connection.
            Pq.reset connection
            putMVar var (ConnectionState.resetPreparedStatementsCache connectionState)

        throwIO exception
      Right (result, !newState) -> do
        putMVar var newState
        pure result

-- | Drain all pending results from the connection.
drainResults :: Pq.Connection -> IO ()
drainResults conn = do
  mResult <- Pq.getResult conn
  case mResult of
    Nothing -> pure ()
    Just _ -> drainResults conn
