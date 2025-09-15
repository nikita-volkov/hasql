module ConnectionSpec (spec) where

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
