module TestingKit.Testcontainers where

import Control.Exception
import Control.Monad
import Data.Bool
import Hasql.Connection qualified
import Hasql.Connection.Setting qualified
import Hasql.Connection.Setting.Connection qualified
import Hasql.Connection.Setting.Connection.Param qualified
import Test.Hspec qualified as Hspec
import TestcontainersPostgresql qualified
import Prelude

withConnectionSettings :: TestcontainersPostgresql.Distro -> ([Hasql.Connection.Setting.Setting] -> IO ()) -> IO ()
withConnectionSettings distro action = do
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
          distro,
          auth = TestcontainersPostgresql.TrustAuth
        }

withConnection :: (Hasql.Connection.Connection -> IO ()) -> IO ()
withConnection = withConnectionByDistro TestcontainersPostgresql.Distro17

withConnectionByDistro :: TestcontainersPostgresql.Distro -> (Hasql.Connection.Connection -> IO ()) -> IO ()
withConnectionByDistro distro action = withConnectionSettings distro \settings -> do
  connection <- Hasql.Connection.acquire settings
  case connection of
    Left err -> fail ("Connection failed: " <> show err)
    Right conn -> finally (action conn) (Hasql.Connection.release conn)

-- * Hspec

-- | Run with a hardcoded preset of distros.
aroundSpecWithConnectionSettings ::
  -- | Whether to provide an isolated environment for each test.
  Bool ->
  Hspec.SpecWith [Hasql.Connection.Setting.Setting] ->
  Hspec.Spec
aroundSpecWithConnectionSettings isolated spec =
  Hspec.parallel do
    forM_ distros \distro -> do
      describeByDistro distro do
        bool Hspec.aroundAll Hspec.around isolated (withConnectionSettings distro) spec

-- | Run with a hardcoded preset of distros.
aroundSpecWithConnection ::
  -- | Whether to provide an isolated environment for each test.
  Bool ->
  Hspec.SpecWith Hasql.Connection.Connection ->
  Hspec.Spec
aroundSpecWithConnection isolated spec =
  Hspec.parallel do
    forM_ distros \distro -> do
      describeByDistro distro do
        bool Hspec.aroundAll Hspec.around isolated (withConnectionByDistro distro) spec

distros :: [TestcontainersPostgresql.Distro]
distros =
  [ TestcontainersPostgresql.Distro10,
    TestcontainersPostgresql.Distro17
  ]

describeByDistro :: TestcontainersPostgresql.Distro -> Hspec.SpecWith a -> Hspec.SpecWith a
describeByDistro distro =
  Hspec.describe ("postgres:" <> drop 6 (show distro))
