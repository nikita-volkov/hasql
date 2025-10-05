module Features.ConnectionSpec (spec) where

import Hasql.Connection qualified
import Hasql.Connection.Setting qualified
import Hasql.Connection.Setting.Connection qualified
import Hasql.Connection.Setting.Connection.Param qualified
import Test.Hspec
import TestcontainersPostgresql qualified
import Prelude

spec :: Spec
spec = parallel do
  describe "postgres:10" do
    byDistro TestcontainersPostgresql.Distro10
  describe "postgres:17" do
    byDistro TestcontainersPostgresql.Distro17

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
    describe "NetworkingAcquisitionError" do
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
          Left (Hasql.Connection.NetworkingAcquisitionError _) -> pure ()
          Left err -> expectationFailure ("Expected NetworkingAcquisitionError, got: " <> show err)
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
          Left (Hasql.Connection.NetworkingAcquisitionError _) -> pure ()
          Left err -> expectationFailure ("Expected NetworkingAcquisitionError, got: " <> show err)
          Right _conn -> expectationFailure "Expected connection to fail"

    describe "AuthenticationAcquisitionError" do
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
                Left (Hasql.Connection.AuthenticationAcquisitionError _) -> pure ()
                Left err -> expectationFailure ("Expected AuthenticationAcquisitionError, got: " <> show err)
                Right _conn -> expectationFailure "Expected connection to fail with authentication error"
          )
