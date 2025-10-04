module Features.ServerVersionCompatibilitySpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Connection.Setting qualified as Setting
import Hasql.Connection.Setting.Connection qualified as ConnectionSetting
import Hasql.Connection.Setting.Connection.Param qualified as Param
import Test.Hspec
import TestcontainersPostgresql qualified
import Prelude

spec :: Spec
spec = parallel do
  describe "Server Version Compatibility" do
    describe "postgres:9" do
      testVersionCompatibility TestcontainersPostgresql.Distro9 (Just "Server version is lower than 10: 9.6.24")
    describe "postgres:10" do
      testVersionCompatibility TestcontainersPostgresql.Distro10 Nothing
    describe "postgres:17" do
      testVersionCompatibility TestcontainersPostgresql.Distro17 Nothing

testVersionCompatibility :: TestcontainersPostgresql.Distro -> Maybe Text -> Spec
testVersionCompatibility distro expectedError = do
  it
    ( "connection" <> case expectedError of
        Just err -> " fails with error: " <> show err
        Nothing -> " succeeds"
    )
    do
      TestcontainersPostgresql.run
        ( TestcontainersPostgresql.Config
            { forwardLogs = False,
              distro,
              auth = TestcontainersPostgresql.TrustAuth
            }
        )
        ( \(host, port) -> do
            let settings =
                  [ Setting.connection
                      ( ConnectionSetting.params
                          [ Param.host host,
                            Param.port (fromIntegral port),
                            Param.user "postgres",
                            Param.password "",
                            Param.dbname "postgres"
                          ]
                      )
                  ]
            result <- Connection.acquire settings
            case result of
              Right conn -> do
                Connection.release conn
                case expectedError of
                  Just err -> expectationFailure ("Expected connection to fail with error: " <> show err <> ", but it succeeded")
                  Nothing -> pure ()
              Left err -> do
                case expectedError of
                  Just expectedErr -> err `shouldBe` Connection.CompatibilityAcquisitionError expectedErr
                  Nothing -> expectationFailure ("Expected connection to succeed, but it failed with error: " <> show err)
        )
