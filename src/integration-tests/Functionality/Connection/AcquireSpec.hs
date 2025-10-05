module Functionality.Connection.AcquireSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Connection.Setting qualified as Setting
import Hasql.Connection.Setting.Connection qualified as ConnectionSetting
import Hasql.Connection.Setting.Connection.Param qualified as Param
import Test.Hspec
import TestcontainersPostgresql qualified
import Prelude

spec :: Spec
spec = do
  describe "By result" do
    describe "Left" do
      describe "Networking" do
        it "Fails on server missing" do
          let settings =
                [ Setting.connection
                    ( ConnectionSetting.params
                        [ Param.host "nopostgresql.net",
                          Param.port 5432
                        ]
                    )
                ]
          result <- Connection.acquire settings
          case result of
            Right conn -> do
              Connection.release conn
              expectationFailure "Expected connection to fail with authentication error, but it succeeded"
            Left (Connection.NetworkingAcquisitionError _) ->
              pure ()
            Left err ->
              expectationFailure ("Expected NetworkingAcquisitionError, but got: " <> show err)

  describe "postgres:9" do
    itFails TestcontainersPostgresql.Distro9 (Connection.CompatibilityAcquisitionError "Server version is lower than 10: 9.6.24")

  describe "postgres:10" do
    itSucceeds TestcontainersPostgresql.Distro10

  describe "postgres:17" do
    let distro = TestcontainersPostgresql.Distro17

    itSucceeds TestcontainersPostgresql.Distro17

    it "Fails with authentication error on wrong password" do
      TestcontainersPostgresql.run
        TestcontainersPostgresql.Config
          { forwardLogs = False,
            distro,
            auth = TestcontainersPostgresql.CredentialsAuth "postgres" "postgres"
          }
        \(host, port) -> do
          let settings =
                [ Setting.connection
                    ( ConnectionSetting.params
                        [ Param.host host,
                          Param.port (fromIntegral port),
                          Param.user "postgres",
                          Param.password "",
                          Param.dbname "postgres1"
                        ]
                    )
                ]
          result <- Connection.acquire settings
          case result of
            Right conn -> do
              Connection.release conn
              expectationFailure "Expected connection to fail with authentication error, but it succeeded"
            Left (Connection.AuthenticationAcquisitionError _) ->
              pure ()
            Left err ->
              expectationFailure ("Expected AuthenticationAcquisitionError, but got: " <> show err)

    it "Fails with authentication error on wrong user" do
      TestcontainersPostgresql.run
        TestcontainersPostgresql.Config
          { forwardLogs = False,
            distro,
            auth = TestcontainersPostgresql.CredentialsAuth "postgres" "postgres"
          }
        \(host, port) -> do
          let settings =
                [ Setting.connection
                    ( ConnectionSetting.params
                        [ Param.host host,
                          Param.port (fromIntegral port),
                          Param.user "postgres1",
                          Param.password "",
                          Param.dbname "postgres"
                        ]
                    )
                ]
          result <- Connection.acquire settings
          case result of
            Right conn -> do
              Connection.release conn
              expectationFailure "Expected connection to fail with authentication error, but it succeeded"
            Left (Connection.AuthenticationAcquisitionError _) ->
              pure ()
            Left err ->
              expectationFailure ("Expected AuthenticationAcquisitionError, but got: " <> show err)

itFails :: TestcontainersPostgresql.Distro -> Connection.AcquisitionError -> Spec
itFails distro expectedError =
  let config =
        TestcontainersPostgresql.Config
          { forwardLogs = False,
            distro,
            auth = TestcontainersPostgresql.TrustAuth
          }
   in it "Fails with error" do
        TestcontainersPostgresql.run config \(host, port) -> do
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
              expectationFailure ("Expected connection to fail with error: " <> show expectedError <> ", but it succeeded")
            Left err ->
              err `shouldBe` expectedError

itSucceeds :: TestcontainersPostgresql.Distro -> Spec
itSucceeds distro =
  let config =
        TestcontainersPostgresql.Config
          { forwardLogs = False,
            distro,
            auth = TestcontainersPostgresql.TrustAuth
          }
   in it "Succeeds" do
        TestcontainersPostgresql.run config \(host, port) -> do
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
            Left err -> do
              expectationFailure ("Expected connection to succeed, but it failed with error: " <> show err)
