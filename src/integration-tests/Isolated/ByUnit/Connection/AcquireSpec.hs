module Isolated.ByUnit.Connection.AcquireSpec (spec) where

import Hasql.Connection qualified
import Hasql.Connection qualified as Connection
import Hasql.Connection.Setting qualified
import Hasql.Connection.Setting qualified as Setting
import Hasql.Connection.Setting.Connection qualified
import Hasql.Connection.Setting.Connection qualified as ConnectionSetting
import Hasql.Connection.Setting.Connection.Param qualified
import Hasql.Connection.Setting.Connection.Param qualified as Param
import Hasql.Errors qualified as Errors
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
            Left (Errors.NetworkingConnectionError _) ->
              pure ()
            Left err ->
              expectationFailure ("Expected NetworkingConnectionError, but got: " <> show err)

  describe "postgres:9" do
    itFails TestcontainersPostgresql.Distro9 (Errors.CompatibilityConnectionError "Server version is lower than 10: 9.6.24")

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
            Left (Errors.AuthenticationConnectionError _) ->
              pure ()
            Left err ->
              expectationFailure ("Expected AuthenticationConnectionError, but got: " <> show err)

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
            Left (Errors.AuthenticationConnectionError _) ->
              pure ()
            Left err ->
              expectationFailure ("Expected AuthenticationConnectionError, but got: " <> show err)

  describe "postgres:10" do
    byDistro TestcontainersPostgresql.Distro10

  describe "postgres:17" do
    byDistro TestcontainersPostgresql.Distro17

itFails :: TestcontainersPostgresql.Distro -> Errors.ConnectionError -> Spec
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

byDistro :: TestcontainersPostgresql.Distro -> Spec
byDistro distro = do
  let itConnects :: Text -> Text -> Spec
      itConnects username password =
        describe ("username: " <> toList username) do
          describe ("password: " <> toList password) do
            it "connects" do
              TestcontainersPostgresql.run
                ( TestcontainersPostgresql.Config
                    { forwardLogs = False,
                      distro,
                      auth = TestcontainersPostgresql.CredentialsAuth username password
                    }
                )
                ( \(host, port) -> do
                    result <-
                      Hasql.Connection.acquire
                        [ Hasql.Connection.Setting.connection
                            ( Hasql.Connection.Setting.Connection.params
                                [ Hasql.Connection.Setting.Connection.Param.host host,
                                  Hasql.Connection.Setting.Connection.Param.port (fromIntegral port),
                                  Hasql.Connection.Setting.Connection.Param.user username,
                                  Hasql.Connection.Setting.Connection.Param.password password
                                ]
                            )
                        ]
                    case result of
                      Left err -> expectationFailure ("Connection failed: " <> show err <> ". Host: " <> show host <> ", port: " <> show port)
                      Right connection -> do
                        Hasql.Connection.release connection
                        pure ()
                )
   in do
        itConnects "user" "new password"
        itConnects "user" "new\\password"
        itConnects "user" "new'password"
        itConnects "new user" "password"

  describe "Connection errors" do
    describe "NetworkingConnectionError" do
      it "is reported for invalid host" do
        result <-
          Hasql.Connection.acquire
            [ Hasql.Connection.Setting.connection
                ( Hasql.Connection.Setting.Connection.params
                    [ Hasql.Connection.Setting.Connection.Param.host "nonexistent.invalid.host",
                      Hasql.Connection.Setting.Connection.Param.port 5432,
                      Hasql.Connection.Setting.Connection.Param.user "postgres",
                      Hasql.Connection.Setting.Connection.Param.password ""
                    ]
                )
            ]
        case result of
          Left (Errors.NetworkingConnectionError _) -> pure ()
          Left err -> expectationFailure ("Expected NetworkingConnectionError, got: " <> show err)
          Right _conn -> expectationFailure "Expected connection to fail"

      it "is reported for connection refused" do
        result <-
          Hasql.Connection.acquire
            [ Hasql.Connection.Setting.connection
                ( Hasql.Connection.Setting.Connection.params
                    [ Hasql.Connection.Setting.Connection.Param.host "127.0.0.1",
                      Hasql.Connection.Setting.Connection.Param.port 1,
                      Hasql.Connection.Setting.Connection.Param.user "postgres",
                      Hasql.Connection.Setting.Connection.Param.password ""
                    ]
                )
            ]
        case result of
          Left (Errors.NetworkingConnectionError _) -> pure ()
          Left err -> expectationFailure ("Expected NetworkingConnectionError, got: " <> show err)
          Right _conn -> expectationFailure "Expected connection to fail"

    describe "AuthenticationConnectionError" do
      it "is reported for invalid credentials" do
        TestcontainersPostgresql.run
          ( TestcontainersPostgresql.Config
              { forwardLogs = False,
                distro,
                auth = TestcontainersPostgresql.CredentialsAuth "postgres" "correctpassword"
              }
          )
          ( \(host, port) -> do
              result <-
                Hasql.Connection.acquire
                  [ Hasql.Connection.Setting.connection
                      ( Hasql.Connection.Setting.Connection.params
                          [ Hasql.Connection.Setting.Connection.Param.host host,
                            Hasql.Connection.Setting.Connection.Param.port (fromIntegral port),
                            Hasql.Connection.Setting.Connection.Param.user "postgres",
                            Hasql.Connection.Setting.Connection.Param.password "wrongpassword"
                          ]
                      )
                  ]
              case result of
                Left (Errors.AuthenticationConnectionError _) -> pure ()
                Left err -> expectationFailure ("Expected AuthenticationConnectionError, got: " <> show err)
                Right _conn -> expectationFailure "Expected connection to fail with authentication error"
          )
