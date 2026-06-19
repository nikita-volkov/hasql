module Isolated.ByUnit.Connection.AcquireSpec (spec) where

import Hasql.Connection qualified
import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Hasql.Errors qualified as Errors
import Helpers.Scripts qualified as Scripts
import Pqi.Ffi qualified as Pqi.Ffi
import Pqi.Native qualified as Pqi.Native
import Test.Hspec
import TestcontainersPostgresql qualified
import Prelude

spec :: Spec
spec = forM_ adapters \(adapterName, acquire) ->
  describe adapterName (specWith acquire)
  where
    adapters =
      [ ("pqi-ffi", Connection.acquire (Proxy @Pqi.Ffi.Connection)),
        ("pqi-native", Connection.acquire (Proxy @Pqi.Native.Connection))
      ]

specWith :: Scripts.Acquire -> Spec
specWith acquire = do
  describe "By result" do
    describe "Left" do
      describe "Networking" do
        it "Fails on server missing" do
          let settings =
                Settings.hostAndPort "nopostgresql.net" 5432
          result <- acquire settings
          case result of
            Right conn -> do
              Connection.release conn
              expectationFailure "Expected connection to fail with authentication error, but it succeeded"
            Left (Errors.NetworkingConnectionError _) ->
              pure ()
            Left err ->
              expectationFailure ("Expected NetworkingConnectionError, but got: " <> show err)

  describe "postgres:9" do
    it "Succeeds" do
      TestcontainersPostgresql.run
        TestcontainersPostgresql.Config
          { tagName = "postgres:9",
            auth = TestcontainersPostgresql.CredentialsAuth "postgres" "postgres",
            forwardLogs = False
          }
        \(host, port) -> do
          let settings =
                mconcat
                  [ Settings.hostAndPort host port,
                    Settings.user "postgres",
                    Settings.password "postgres",
                    Settings.dbname "postgres"
                  ]
          result <- acquire settings
          case result of
            Right conn -> do
              Connection.release conn
            Left err -> do
              expectationFailure ("Expected connection to succeed, but it failed with error: " <> show err)

  describe "postgres:18" do
    it "Succeeds" do
      TestcontainersPostgresql.run
        TestcontainersPostgresql.Config
          { tagName = "postgres:18",
            auth = TestcontainersPostgresql.CredentialsAuth "postgres" "postgres",
            forwardLogs = False
          }
        \(host, port) -> do
          let settings =
                mconcat
                  [ Settings.hostAndPort host port,
                    Settings.user "postgres",
                    Settings.password "postgres",
                    Settings.dbname "postgres"
                  ]
          result <- acquire settings
          case result of
            Right conn -> do
              Connection.release conn
            Left err -> do
              expectationFailure ("Expected connection to succeed, but it failed with error: " <> show err)

    it "Fails with authentication error on incorrect password" do
      TestcontainersPostgresql.run
        TestcontainersPostgresql.Config
          { tagName = "postgres:18",
            auth = TestcontainersPostgresql.CredentialsAuth "postgres" "postgres",
            forwardLogs = False
          }
        \(host, port) -> do
          let settings =
                mconcat
                  [ Settings.hostAndPort host port,
                    Settings.user "postgres",
                    Settings.password "",
                    Settings.dbname "postgres1"
                  ]
          result <- acquire settings
          case result of
            Right conn -> do
              Connection.release conn
              expectationFailure "Expected connection to fail with authentication error, but it succeeded"
            Left (Errors.AuthenticationConnectionError _) ->
              pure ()
            Left err ->
              expectationFailure ("Expected AuthenticationConnectionError, but got: " <> show err)

    it "Fails with authentication error on incorrect user" do
      TestcontainersPostgresql.run
        TestcontainersPostgresql.Config
          { tagName = "postgres:18",
            auth = TestcontainersPostgresql.CredentialsAuth "postgres" "postgres",
            forwardLogs = False
          }
        \(host, port) -> do
          let settings =
                mconcat
                  [ Settings.hostAndPort host port,
                    Settings.user "postgres1",
                    Settings.password "",
                    Settings.dbname "postgres"
                  ]
          result <- acquire settings
          case result of
            Right conn -> do
              Connection.release conn
              expectationFailure "Expected connection to fail with authentication error, but it succeeded"
            Left (Errors.AuthenticationConnectionError _) ->
              pure ()
            Left err ->
              expectationFailure ("Expected AuthenticationConnectionError, but got: " <> show err)

  describe "postgres:9" do
    byDistro acquire "postgres:9"

  describe "postgres:18" do
    byDistro acquire "postgres:18"

byDistro :: Scripts.Acquire -> Text -> Spec
byDistro acquire tagName = do
  let itConnects :: Text -> Text -> Spec
      itConnects username password =
        describe ("username: " <> toList username) do
          describe ("password: " <> toList password) do
            it "connects" do
              TestcontainersPostgresql.run
                TestcontainersPostgresql.Config
                  { tagName,
                    auth = TestcontainersPostgresql.CredentialsAuth username password,
                    forwardLogs = False
                  }
                ( \(host, port) -> do
                    result <-
                      acquire
                        ( mconcat
                            [ Settings.hostAndPort host port,
                              Settings.user username,
                              Settings.password password
                            ]
                        )
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
          acquire
            ( mconcat
                [ Settings.hostAndPort "nonexistent.invalid.host" 5432,
                  Settings.user "postgres",
                  Settings.password ""
                ]
            )
        case result of
          Left (Errors.NetworkingConnectionError _) -> pure ()
          Left err -> expectationFailure ("Expected NetworkingConnectionError, got: " <> show err)
          Right _conn -> expectationFailure "Expected connection to fail"

      it "is reported for connection refused" do
        result <-
          acquire
            ( mconcat
                [ Settings.hostAndPort "127.0.0.1" 1,
                  Settings.user "postgres",
                  Settings.password ""
                ]
            )
        case result of
          Left (Errors.NetworkingConnectionError _) -> pure ()
          Left err -> expectationFailure ("Expected NetworkingConnectionError, got: " <> show err)
          Right _conn -> expectationFailure "Expected connection to fail"

    describe "AuthenticationConnectionError" do
      it "is reported for invalid credentials" do
        TestcontainersPostgresql.run
          TestcontainersPostgresql.Config
            { tagName,
              auth = TestcontainersPostgresql.CredentialsAuth "password" "correctpassword",
              forwardLogs = False
            }
          \(host, port) -> do
            result <-
              acquire
                ( mconcat
                    [ Settings.hostAndPort host port,
                      Settings.user "incorrectuser",
                      Settings.password "incorrectpassword"
                    ]
                )
            case result of
              Left (Errors.AuthenticationConnectionError _) -> pure ()
              Left err -> expectationFailure ("Expected AuthenticationConnectionError, got: " <> show err)
              Right _conn -> expectationFailure "Expected connection to fail with authentication error"
