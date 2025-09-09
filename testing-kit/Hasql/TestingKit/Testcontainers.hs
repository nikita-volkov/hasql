module Hasql.TestingKit.Testcontainers where

import Control.Exception
import Hasql.Connection qualified
import Hasql.Connection.Setting qualified
import Hasql.Connection.Setting.Connection qualified
import Hasql.Connection.Setting.Connection.Param qualified
import TestcontainersPostgresql qualified
import Prelude

withConnectionSettings :: ([Hasql.Connection.Setting.Setting] -> IO ()) -> IO ()
withConnectionSettings action = do
  TestcontainersPostgresql.run config \(host, port) -> do
    action
      [ Hasql.Connection.Setting.connection
          ( Hasql.Connection.Setting.Connection.params
              [ Hasql.Connection.Setting.Connection.Param.host host,
                Hasql.Connection.Setting.Connection.Param.port (fromIntegral port),
                Hasql.Connection.Setting.Connection.Param.user "postgres",
                Hasql.Connection.Setting.Connection.Param.password "",
                Hasql.Connection.Setting.Connection.Param.dbname "postgres"
              ]
          )
      ]
  where
    config =
      TestcontainersPostgresql.Config
        { forwardLogs = False,
          distro = TestcontainersPostgresql.Distro17,
          auth = TestcontainersPostgresql.TrustAuth
        }

withConnection :: (Hasql.Connection.Connection -> IO ()) -> IO ()
withConnection action = withConnectionSettings $ \settings -> do
  connection <- Hasql.Connection.acquire settings
  case connection of
    Left err -> fail ("Connection failed: " <> show err)
    Right conn -> finally (action conn) (Hasql.Connection.release conn)
