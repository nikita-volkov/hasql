module Hasql.TestingKit.TestcontainersHelpers where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Hasql.Connection qualified as Connection
import Hasql.Connection.Setting qualified as Setting
import Hasql.Connection.Setting.Connection qualified as Setting.Connection
import Hasql.Connection.Setting.Connection.Param qualified as Setting.Connection.Param
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Hasql.TestingKit.Preludes.Base
import TestcontainersPostgresql qualified

-- | Default testcontainer config for PostgreSQL
defaultTestcontainerConfig :: TestcontainersPostgresql.Config
defaultTestcontainerConfig =
  TestcontainersPostgresql.Config
    { forwardLogs = False,
      distro = TestcontainersPostgresql.Distro17,
      auth = TestcontainersPostgresql.CredentialsAuth "postgres" "postgres"
    }

-- | Create connection settings from host and port
connectionSettingsFromHostPort :: Text -> Int -> [Setting.Setting]
connectionSettingsFromHostPort host port =
  [ Setting.connection
      ( Setting.Connection.params
          [ Setting.Connection.Param.host host,
            Setting.Connection.Param.port (fromIntegral port),
            Setting.Connection.Param.user "postgres",
            Setting.Connection.Param.password "postgres",
            Setting.Connection.Param.dbname "postgres"
          ]
      )
  ]

-- | Run a session with testcontainers
runSessionWithTestcontainers :: Session.Session a -> IO (Either ConnectionOrSessionError a)
runSessionWithTestcontainers session = do
  resultRef <- newIORef (Left $ ConnectionError Nothing) -- dummy initial value
  TestcontainersPostgresql.run defaultTestcontainerConfig $ \(host, port) -> do
    let connectionSettings = connectionSettingsFromHostPort host port
    result <- runExceptT $ do
      connection <- ExceptT $ fmap (first ConnectionError) $ Connection.acquire connectionSettings
      sessionResult <- ExceptT $ fmap (first SessionError) $ Session.run session connection
      lift $ Connection.release connection
      pure sessionResult
    writeIORef resultRef result
  readIORef resultRef

-- | Run a pipeline with testcontainers
runPipelineWithTestcontainers :: Pipeline.Pipeline a -> IO (Either ConnectionOrSessionError a)
runPipelineWithTestcontainers =
  runSessionWithTestcontainers . Session.pipeline

-- | Error type combining connection and session errors
data ConnectionOrSessionError
  = ConnectionError Connection.ConnectionError
  | SessionError Session.SessionError
  deriving (Show, Eq)
