module Hasql.TestingKit.Testcontainers where

import Control.Exception
import Hasql.Connection qualified
import Hasql.Connection.Setting qualified
import Hasql.Connection.Setting.Connection qualified
import Hasql.Connection.Setting.Connection.Param qualified
import TestcontainersPostgresql qualified
import Prelude

withConnection :: (Hasql.Connection.Connection -> IO ()) -> IO ()
withConnection action = do
  TestcontainersPostgresql.run config \(host, port) -> do
    connection <-
      Hasql.Connection.acquire
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
    connection <- case connection of
      Left err -> fail ("Connection failed: " <> show err <> ". Host: " <> show host <> ", port: " <> show port)
      Right connection -> pure connection
    finally
      (action connection)
      (Hasql.Connection.release connection)
  where
    config =
      TestcontainersPostgresql.Config
        { forwardLogs = False,
          distro = TestcontainersPostgresql.Distro17,
          auth = TestcontainersPostgresql.TrustAuth
        }
