module Integration.Isolated.Connection.AcquireSpec (spec) where

import Hasql.Connection qualified
import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Hasql.Errors qualified as Errors
import Pqi qualified
import Prelude
import Test.Hspec
import TestcontainersPostgresql qualified

spec :: SpecWith Pqi.Adapter
spec = do
  describe "By result" do
    describe "Left" do
      describe "Networking" do
        it "Fails on server missing" \adapter -> do
          let settings =
                Settings.hostAndPort "nopostgresql.net" 5432
          result <- Connection.acquire adapter settings
          case result of
            Right conn -> do
              Connection.release conn
              expectationFailure "Expected connection to fail with authentication error, but it succeeded"
            Left (Errors.NetworkingAcquireError _) ->
              pure ()
            Left err ->
              expectationFailure ("Expected NetworkingAcquireError, but got: " <> show err)

  describe "postgres:9" do
    it "Succeeds" \adapter -> do
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
          result <- Connection.acquire adapter settings
          case result of
            Right conn -> do
              Connection.release conn
            Left err -> do
              expectationFailure ("Expected connection to succeed, but it failed with error: " <> show err)

  describe "postgres:18" do
    it "Succeeds" \adapter -> do
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
          result <- Connection.acquire adapter settings
          case result of
            Right conn -> do
              Connection.release conn
            Left err -> do
              expectationFailure ("Expected connection to succeed, but it failed with error: " <> show err)

    it "Fails with authentication error on incorrect password" \adapter -> do
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
          result <- Connection.acquire adapter settings
          case result of
            Right conn -> do
              Connection.release conn
              expectationFailure "Expected connection to fail with authentication error, but it succeeded"
            Left (Errors.AuthenticationAcquireError _) ->
              pure ()
            Left err ->
              expectationFailure ("Expected AuthenticationAcquireError, but got: " <> show err)

    it "Fails with authentication error on incorrect user" \adapter -> do
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
          result <- Connection.acquire adapter settings
          case result of
            Right conn -> do
              Connection.release conn
              expectationFailure "Expected connection to fail with authentication error, but it succeeded"
            Left (Errors.AuthenticationAcquireError _) ->
              pure ()
            Left err ->
              expectationFailure ("Expected AuthenticationAcquireError, but got: " <> show err)

  describe "postgres:9" do
    byDistro "postgres:9"

  describe "postgres:18" do
    byDistro "postgres:18"

byDistro :: Text -> SpecWith Pqi.Adapter
byDistro tagName = do
  let itConnects :: Text -> Text -> SpecWith Pqi.Adapter
      itConnects username password =
        describe ("username: " <> toList username) do
          describe ("password: " <> toList password) do
            it "connects" \adapter -> do
              TestcontainersPostgresql.run
                TestcontainersPostgresql.Config
                  { tagName,
                    auth = TestcontainersPostgresql.CredentialsAuth username password,
                    forwardLogs = False
                  }
                ( \(host, port) -> do
                    result <-
                      Hasql.Connection.acquire
                        adapter
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
    describe "NetworkingAcquireError" do
      it "is reported for invalid host" \adapter -> do
        result <-
          Hasql.Connection.acquire
            adapter
            ( mconcat
                [ Settings.hostAndPort "nonexistent.invalid.host" 5432,
                  Settings.user "postgres",
                  Settings.password ""
                ]
            )
        case result of
          Left (Errors.NetworkingAcquireError _) -> pure ()
          Left err -> expectationFailure ("Expected NetworkingAcquireError, got: " <> show err)
          Right _conn -> expectationFailure "Expected connection to fail"

      it "is reported for connection refused" \adapter -> do
        result <-
          Hasql.Connection.acquire
            adapter
            ( mconcat
                [ Settings.hostAndPort "127.0.0.1" 1,
                  Settings.user "postgres",
                  Settings.password ""
                ]
            )
        case result of
          Left (Errors.NetworkingAcquireError _) -> pure ()
          Left err -> expectationFailure ("Expected NetworkingAcquireError, got: " <> show err)
          Right _conn -> expectationFailure "Expected connection to fail"

    describe "AuthenticationAcquireError" do
      it "is reported for invalid credentials" \adapter -> do
        TestcontainersPostgresql.run
          TestcontainersPostgresql.Config
            { tagName,
              auth = TestcontainersPostgresql.CredentialsAuth "password" "correctpassword",
              forwardLogs = False
            }
          \(host, port) -> do
            result <-
              Hasql.Connection.acquire
                adapter
                ( mconcat
                    [ Settings.hostAndPort host port,
                      Settings.user "incorrectuser",
                      Settings.password "incorrectpassword"
                    ]
                )
            case result of
              Left (Errors.AuthenticationAcquireError _) -> pure ()
              Left err -> expectationFailure ("Expected AuthenticationAcquireError, got: " <> show err)
              Right _conn -> expectationFailure "Expected connection to fail with authentication error"
