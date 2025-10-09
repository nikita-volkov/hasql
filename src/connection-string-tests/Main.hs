module Main where

import ConnectionString qualified
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Platform.Prelude
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Classes qualified as Laws

main :: IO ()
main = hspec do
  describe "ConnectionString" do
    describe "roundtrip property" do
      it "toUrl . parseText is identity for valid connection strings" do
        property \connStr ->
          let url = ConnectionString.toUrl connStr
           in case ConnectionString.parseText url of
                Left err -> counterexample ("Parse error: " <> Text.unpack err <> "\nURL: " <> Text.unpack url) False
                Right parsed -> parsed === connStr

    describe "toUrl" do
      it "generates valid postgresql:// URLs" do
        property \connStr ->
          let url = ConnectionString.toUrl connStr
              urlStr = Text.unpack url
           in (url `elem` ["postgresql://", "postgres://"] || "postgresql://" `isPrefixOf` urlStr)
                & counterexample ("Generated URL: " <> Text.unpack url)

      it "encodes user correctly" do
        let connStr = ConnectionString.user "myuser"
            url = ConnectionString.toUrl connStr
        url `shouldBe` "postgresql://myuser@"

      it "encodes user and password correctly" do
        let connStr = mconcat [ConnectionString.user "myuser", ConnectionString.password "secret"]
            url = ConnectionString.toUrl connStr
        url `shouldBe` "postgresql://myuser:secret@"

      it "encodes host correctly" do
        let connStr = ConnectionString.hostAndPort "localhost" Nothing
            url = ConnectionString.toUrl connStr
        url `shouldBe` "postgresql://localhost"

      it "encodes host with port correctly" do
        let connStr = ConnectionString.hostAndPort "localhost" (Just 5433)
            url = ConnectionString.toUrl connStr
        url `shouldBe` "postgresql://localhost:5433"

      it "encodes multiple hosts correctly" do
        let connStr = mconcat [ConnectionString.hostAndPort "host1" (Just 123), ConnectionString.hostAndPort "host2" (Just 456)]
            url = ConnectionString.toUrl connStr
        url `shouldBe` "postgresql://host1:123,host2:456"

      it "encodes database name correctly" do
        let connStr = mconcat [ConnectionString.hostAndPort "localhost" Nothing, ConnectionString.dbname "mydb"]
            url = ConnectionString.toUrl connStr
        url `shouldBe` "postgresql://localhost/mydb"

      it "encodes parameters correctly" do
        let connStr = mconcat [ConnectionString.param "key1" "value1", ConnectionString.param "key2" "value2"]
            url = ConnectionString.toUrl connStr
        url `elem` ["postgresql://?key1=value1&key2=value2", "postgresql://?key2=value2&key1=value1"] `shouldBe` True

      it "encodes full connection string correctly" do
        let connStr =
              mconcat
                [ ConnectionString.user "user",
                  ConnectionString.password "secret",
                  ConnectionString.hostAndPort "localhost" (Just 5433),
                  ConnectionString.dbname "mydb",
                  ConnectionString.param "connect_timeout" "10"
                ]
            url = ConnectionString.toUrl connStr
        url `shouldBe` "postgresql://user:secret@localhost:5433/mydb?connect_timeout=10"

    describe "parseText" do
      it "parses minimal URL" do
        ConnectionString.parseText "postgresql://"
          `shouldBe` Right mempty

      it "parses URL with host" do
        ConnectionString.parseText "postgresql://localhost"
          `shouldBe` Right (ConnectionString.hostAndPort "localhost" Nothing)

      it "parses URL with host and port" do
        ConnectionString.parseText "postgresql://localhost:5433"
          `shouldBe` Right (ConnectionString.hostAndPort "localhost" (Just 5433))

      it "parses URL with host and database" do
        ConnectionString.parseText "postgresql://localhost/mydb"
          `shouldBe` Right (mconcat [ConnectionString.hostAndPort "localhost" Nothing, ConnectionString.dbname "mydb"])

      it "parses URL with user" do
        ConnectionString.parseText "postgresql://user@localhost"
          `shouldBe` Right (mconcat [ConnectionString.user "user", ConnectionString.hostAndPort "localhost" Nothing])

      it "parses URL with user and password" do
        ConnectionString.parseText "postgresql://user:secret@localhost"
          `shouldBe` Right (mconcat [ConnectionString.user "user", ConnectionString.password "secret", ConnectionString.hostAndPort "localhost" Nothing])

      it "parses URL with parameters" do
        case ConnectionString.parseText "postgresql://localhost?key1=value1&key2=value2" of
          Left err -> expectationFailure (Text.unpack err)
          Right cs ->
            ConnectionString.toParams cs `shouldBe` Map.fromList [("key1", "value1"), ("key2", "value2")]

      it "parses complex URL" do
        ConnectionString.parseText "postgresql://user:secret@localhost:5433/mydb?connect_timeout=10&application_name=myapp"
          `shouldSatisfy` isRight

      it "parses URL with multiple hosts" do
        case ConnectionString.parseText "postgresql://host1:123,host2:456/mydb" of
          Left err -> expectationFailure (Text.unpack err)
          Right cs ->
            ConnectionString.toHosts cs `shouldBe` [("host1", Just 123), ("host2", Just 456)]

    describe "Laws" do
      laws (Laws.semigroupLaws (Proxy @ConnectionString.ConnectionString))
      laws (Laws.monoidLaws (Proxy @ConnectionString.ConnectionString))

    describe "toKeyValueString" do
      it "generates minimal connection string" do
        let connStr = mempty
        ConnectionString.toKeyValueString connStr `shouldBe` ""

      it "encodes host correctly" do
        let connStr = ConnectionString.hostAndPort "localhost" Nothing
        ConnectionString.toKeyValueString connStr `shouldBe` "host=localhost"

      it "encodes host with port correctly" do
        let connStr = ConnectionString.hostAndPort "localhost" (Just 5433)
        ConnectionString.toKeyValueString connStr `shouldBe` "host=localhost port=5433"

      it "encodes user correctly" do
        let connStr = ConnectionString.user "myuser"
        ConnectionString.toKeyValueString connStr `shouldBe` "user=myuser"

      it "encodes password correctly" do
        let connStr = ConnectionString.password "secret"
        ConnectionString.toKeyValueString connStr `shouldBe` "password=secret"

      it "encodes database name correctly" do
        let connStr = ConnectionString.dbname "mydb"
        ConnectionString.toKeyValueString connStr `shouldBe` "dbname=mydb"

      it "encodes parameters correctly" do
        let connStr = ConnectionString.param "connect_timeout" "10"
        ConnectionString.toKeyValueString connStr `shouldBe` "connect_timeout=10"

      it "encodes full connection string correctly" do
        let connStr =
              mconcat
                [ ConnectionString.hostAndPort "localhost" (Just 5433),
                  ConnectionString.user "user",
                  ConnectionString.password "secret",
                  ConnectionString.dbname "mydb",
                  ConnectionString.param "connect_timeout" "10"
                ]
            result = ConnectionString.toKeyValueString connStr
        result `shouldBe` "host=localhost port=5433 user=user password=secret dbname=mydb connect_timeout=10"

      it "quotes values with spaces" do
        let connStr = ConnectionString.param "application_name" "my app"
        ConnectionString.toKeyValueString connStr `shouldBe` "application_name='my app'"

      it "quotes empty values" do
        let connStr = ConnectionString.user ""
        ConnectionString.toKeyValueString connStr `shouldBe` "user=''"

      it "escapes single quotes in values" do
        let connStr = ConnectionString.password "it's secret"
        ConnectionString.toKeyValueString connStr `shouldBe` "password='it\\'s secret'"

      it "escapes backslashes in values" do
        let connStr = ConnectionString.password "path\\to\\secret"
        ConnectionString.toKeyValueString connStr `shouldBe` "password='path\\\\to\\\\secret'"

      it "quotes values with equals signs" do
        let connStr = ConnectionString.param "options" "--key=value"
        ConnectionString.toKeyValueString connStr `shouldBe` "options='--key=value'"

      it "handles multiple parameters in stable order" do
        let connStr = mconcat [ConnectionString.param "key1" "value1", ConnectionString.param "key2" "value2"]
            result = ConnectionString.toKeyValueString connStr
        -- Map.toList should give us a consistent order
        result `elem` ["key1=value1 key2=value2", "key2=value2 key1=value1"] `shouldBe` True

      it "handles complex escaping scenarios" do
        let connStr = ConnectionString.password "a\\b'c d=e"
            result = ConnectionString.toKeyValueString connStr
        result `shouldBe` "password='a\\\\b\\'c d=e'"

      it "only includes first host (keyword/value format limitation)" do
        let connStr = mconcat [ConnectionString.hostAndPort "host1" (Just 123), ConnectionString.hostAndPort "host2" (Just 456)]
            result = ConnectionString.toKeyValueString connStr
        result `shouldBe` "host=host1 port=123"

    describe "toKeyValueString roundtrip" do
      it "parseText . toKeyValueString is identity for simple connection strings" do
        let connStr = mconcat [ConnectionString.hostAndPort "localhost" (Just 5432), ConnectionString.user "user", ConnectionString.dbname "db"]
            kvString = ConnectionString.toKeyValueString connStr
        case ConnectionString.parseText kvString of
          Left err -> expectationFailure ("Parse error: " <> Text.unpack err <> "\nKV String: " <> Text.unpack kvString)
          Right parsed -> parsed `shouldBe` connStr

      it "parseText . toKeyValueString handles quoted values" do
        let connStr = mconcat [ConnectionString.user "my user", ConnectionString.password "secret"]
            kvString = ConnectionString.toKeyValueString connStr
        case ConnectionString.parseText kvString of
          Left err -> expectationFailure ("Parse error: " <> Text.unpack err <> "\nKV String: " <> Text.unpack kvString)
          Right parsed -> parsed `shouldBe` connStr

      it "parseText . toKeyValueString handles escaped quotes" do
        let connStr = ConnectionString.password "it's a secret"
            kvString = ConnectionString.toKeyValueString connStr
        case ConnectionString.parseText kvString of
          Left err -> expectationFailure ("Parse error: " <> Text.unpack err <> "\nKV String: " <> Text.unpack kvString)
          Right parsed -> parsed `shouldBe` connStr

      it "parseText . toKeyValueString handles escaped backslashes" do
        let connStr = ConnectionString.password "path\\to\\file"
            kvString = ConnectionString.toKeyValueString connStr
        case ConnectionString.parseText kvString of
          Left err -> expectationFailure ("Parse error: " <> Text.unpack err <> "\nKV String: " <> Text.unpack kvString)
          Right parsed -> parsed `shouldBe` connStr

      it "parseText . toKeyValueString handles full connection string" do
        let connStr =
              mconcat
                [ ConnectionString.hostAndPort "localhost" (Just 5433),
                  ConnectionString.user "testuser",
                  ConnectionString.password "secret pass",
                  ConnectionString.dbname "testdb",
                  ConnectionString.param "connect_timeout" "10",
                  ConnectionString.param "application_name" "my app"
                ]
            kvString = ConnectionString.toKeyValueString connStr
        case ConnectionString.parseText kvString of
          Left err -> expectationFailure ("Parse error: " <> Text.unpack err <> "\nKV String: " <> Text.unpack kvString)
          Right parsed -> parsed `shouldBe` connStr

      it "property: parseText . toKeyValueString roundtrips for single-host connection strings" do
        property \connStr ->
          -- Only test connection strings with at most one host, since keyword/value format
          -- doesn't support multiple hosts. Create a new connection string with only first host.
          let hosts = ConnectionString.toHosts connStr
              user = ConnectionString.toUser connStr
              password = ConnectionString.toPassword connStr
              dbname = ConnectionString.toDbname connStr
              params = ConnectionString.toParams connStr
              singleHost = take 1 hosts
              connStrSingleHost =
                mconcat
                  [ foldMap ConnectionString.user user,
                    foldMap ConnectionString.password password,
                    mconcat (map (\(h, p) -> ConnectionString.hostAndPort h p) singleHost),
                    foldMap ConnectionString.dbname dbname,
                    mconcat (map (uncurry ConnectionString.param) (Map.toList params))
                  ]
              kvString = ConnectionString.toKeyValueString connStrSingleHost
              -- Skip empty connection strings as they don't roundtrip
              isEmpty = Text.null kvString
           in not isEmpty ==>
                case ConnectionString.parseText kvString of
                  Left err -> counterexample ("Parse error: " <> Text.unpack err <> "\nKV String: " <> Text.unpack kvString) False
                  Right parsed -> parsed === connStrSingleHost

laws :: Laws.Laws -> Spec
laws (Laws.Laws className props) =
  describe className do
    forM_ props \(propName, prop) ->
      it propName do
        property prop
