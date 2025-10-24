module Isolated.ByUnit.Connection.AcquireSpec (spec) where

import Hasql.Connection qualified
import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
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
                Settings.hostAndPort "nopostgresql.net" 5432
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
    it "Fails with compatibility error" do
      TestcontainersPostgresql.hook "postgres:9" "postgres" "postgres" False \(host, port) -> do
        let settings =
              mconcat
                [ Settings.hostAndPort host port,
                  Settings.user "postgres",
                  Settings.password "postgres",
                  Settings.dbname "postgres"
                ]
        result <- Connection.acquire settings
        case result of
          Right conn -> do
            Connection.release conn
            expectationFailure "Expected connection to fail with compatibility error, but it succeeded"
          Left err ->
            err `shouldBe` Errors.CompatibilityConnectionError "Server version is lower than 10: 9.6.24"

  describe "postgres:10" do
    it "Succeeds" do
      TestcontainersPostgresql.hook "postgres:10" "postgres" "postgres" False \(host, port) -> do
        let settings =
              mconcat
                [ Settings.hostAndPort host port,
                  Settings.user "postgres",
                  Settings.password "postgres",
                  Settings.dbname "postgres"
                ]
        result <- Connection.acquire settings
        case result of
          Right conn -> do
            Connection.release conn
          Left err -> do
            expectationFailure ("Expected connection to succeed, but it failed with error: " <> show err)

  describe "postgres:17" do
    it "Succeeds" do
      TestcontainersPostgresql.hook "postgres:17" "postgres" "postgres" False \(host, port) -> do
        let settings =
              mconcat
                [ Settings.hostAndPort host port,
                  Settings.user "postgres",
                  Settings.password "postgres",
                  Settings.dbname "postgres"
                ]
        result <- Connection.acquire settings
        case result of
          Right conn -> do
            Connection.release conn
          Left err -> do
            expectationFailure ("Expected connection to succeed, but it failed with error: " <> show err)

    it "Fails with authentication error on wrong password" do
      TestcontainersPostgresql.hook "postgres:17" "postgres" "postgres" False \(host, port) -> do
        let settings =
              mconcat
                [ Settings.hostAndPort host port,
                  Settings.user "postgres",
                  Settings.password "",
                  Settings.dbname "postgres1"
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
      TestcontainersPostgresql.hook "postgres:17" "postgres" "postgres" False \(host, port) -> do
        let settings =
              mconcat
                [ Settings.hostAndPort host port,
                  Settings.user "postgres1",
                  Settings.password "",
                  Settings.dbname "postgres"
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
    byDistro "postgres:10"

  describe "postgres:17" do
    byDistro "postgres:17"

byDistro :: Text -> Spec
byDistro tagName = do
  let itConnects :: Text -> Text -> Spec
      itConnects username password =
        describe ("username: " <> toList username) do
          describe ("password: " <> toList password) do
            it "connects" do
              TestcontainersPostgresql.hook
                tagName
                username
                password
                False
                ( \(host, port) -> do
                    result <-
                      Hasql.Connection.acquire
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
          Hasql.Connection.acquire
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
          Hasql.Connection.acquire
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
        TestcontainersPostgresql.hook tagName "postgres" "correctpassword" False \(host, port) -> do
          result <-
            Hasql.Connection.acquire
              ( mconcat
                  [ Settings.hostAndPort host port,
                    Settings.user "wronguser",
                    Settings.password "wrongpassword"
                  ]
              )
          case result of
            Left (Errors.AuthenticationConnectionError _) -> pure ()
            Left err -> expectationFailure ("Expected AuthenticationConnectionError, got: " <> show err)
            Right _conn -> expectationFailure "Expected connection to fail with authentication error"
