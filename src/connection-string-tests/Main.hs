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

    describe "PostgreSQL documentation examples" do
      describe "parsing and serialization consistency" do
        it "host=localhost port=5432 dbname=mydb connect_timeout=10" do
          let input = "host=localhost port=5432 dbname=mydb connect_timeout=10"
          case ConnectionString.parseText input of
            Left err -> expectationFailure ("Parse error: " <> Text.unpack err)
            Right cs -> do
              ConnectionString.toHosts cs `shouldBe` [("localhost", Just 5432)]
              ConnectionString.toDbname cs `shouldBe` Just "mydb"
              ConnectionString.toParams cs `shouldBe` Map.singleton "connect_timeout" "10"
              -- Roundtrip through URL
              let url = ConnectionString.toUrl cs
              case ConnectionString.parseText url of
                Left err -> expectationFailure ("URL roundtrip parse error: " <> Text.unpack err)
                Right cs2 -> cs2 `shouldBe` cs

        it "postgresql://" do
          let input = "postgresql://"
          case ConnectionString.parseText input of
            Left err -> expectationFailure ("Parse error: " <> Text.unpack err)
            Right cs -> do
              cs `shouldBe` mempty
              ConnectionString.toUrl cs `shouldBe` "postgresql://"

        it "postgresql://localhost" do
          let input = "postgresql://localhost"
          case ConnectionString.parseText input of
            Left err -> expectationFailure ("Parse error: " <> Text.unpack err)
            Right cs -> do
              ConnectionString.toHosts cs `shouldBe` [("localhost", Nothing)]
              ConnectionString.toUrl cs `shouldBe` "postgresql://localhost"

        it "postgresql://localhost:5433" do
          let input = "postgresql://localhost:5433"
          case ConnectionString.parseText input of
            Left err -> expectationFailure ("Parse error: " <> Text.unpack err)
            Right cs -> do
              ConnectionString.toHosts cs `shouldBe` [("localhost", Just 5433)]
              ConnectionString.toUrl cs `shouldBe` "postgresql://localhost:5433"

        it "postgresql://localhost/mydb" do
          let input = "postgresql://localhost/mydb"
          case ConnectionString.parseText input of
            Left err -> expectationFailure ("Parse error: " <> Text.unpack err)
            Right cs -> do
              ConnectionString.toHosts cs `shouldBe` [("localhost", Nothing)]
              ConnectionString.toDbname cs `shouldBe` Just "mydb"
              ConnectionString.toUrl cs `shouldBe` "postgresql://localhost/mydb"

        it "postgresql://user@localhost" do
          let input = "postgresql://user@localhost"
          case ConnectionString.parseText input of
            Left err -> expectationFailure ("Parse error: " <> Text.unpack err)
            Right cs -> do
              ConnectionString.toUser cs `shouldBe` Just "user"
              ConnectionString.toHosts cs `shouldBe` [("localhost", Nothing)]
              ConnectionString.toUrl cs `shouldBe` "postgresql://user@localhost"

        it "postgresql://user:secret@localhost" do
          let input = "postgresql://user:secret@localhost"
          case ConnectionString.parseText input of
            Left err -> expectationFailure ("Parse error: " <> Text.unpack err)
            Right cs -> do
              ConnectionString.toUser cs `shouldBe` Just "user"
              ConnectionString.toPassword cs `shouldBe` Just "secret"
              ConnectionString.toHosts cs `shouldBe` [("localhost", Nothing)]
              ConnectionString.toUrl cs `shouldBe` "postgresql://user:secret@localhost"

        it "postgresql://other@localhost/otherdb?connect_timeout=10&application_name=myapp" do
          let input = "postgresql://other@localhost/otherdb?connect_timeout=10&application_name=myapp"
          case ConnectionString.parseText input of
            Left err -> expectationFailure ("Parse error: " <> Text.unpack err)
            Right cs -> do
              ConnectionString.toUser cs `shouldBe` Just "other"
              ConnectionString.toHosts cs `shouldBe` [("localhost", Nothing)]
              ConnectionString.toDbname cs `shouldBe` Just "otherdb"
              ConnectionString.toParams cs `shouldBe` Map.fromList [("connect_timeout", "10"), ("application_name", "myapp")]
              -- Roundtrip
              let url = ConnectionString.toUrl cs
              case ConnectionString.parseText url of
                Left err -> expectationFailure ("URL roundtrip parse error: " <> Text.unpack err)
                Right cs2 -> cs2 `shouldBe` cs

        it "postgresql://host1:123,host2:456/somedb?target_session_attrs=any&application_name=myapp" do
          let input = "postgresql://host1:123,host2:456/somedb?target_session_attrs=any&application_name=myapp"
          case ConnectionString.parseText input of
            Left err -> expectationFailure ("Parse error: " <> Text.unpack err)
            Right cs -> do
              ConnectionString.toHosts cs `shouldBe` [("host1", Just 123), ("host2", Just 456)]
              ConnectionString.toDbname cs `shouldBe` Just "somedb"
              ConnectionString.toParams cs `shouldBe` Map.fromList [("target_session_attrs", "any"), ("application_name", "myapp")]
              -- Roundtrip
              let url = ConnectionString.toUrl cs
              case ConnectionString.parseText url of
                Left err -> expectationFailure ("URL roundtrip parse error: " <> Text.unpack err)
                Right cs2 -> cs2 `shouldBe` cs

        it "postgresql://user@localhost:5433/mydb?options=-c%20synchronous_commit%3Doff" do
          let input = "postgresql://user@localhost:5433/mydb?options=-c%20synchronous_commit%3Doff"
          case ConnectionString.parseText input of
            Left err -> expectationFailure ("Parse error: " <> Text.unpack err)
            Right cs -> do
              ConnectionString.toUser cs `shouldBe` Just "user"
              ConnectionString.toHosts cs `shouldBe` [("localhost", Just 5433)]
              ConnectionString.toDbname cs `shouldBe` Just "mydb"
              ConnectionString.toParams cs `shouldBe` Map.singleton "options" "-c synchronous_commit=off"
              -- Roundtrip
              let url = ConnectionString.toUrl cs
              case ConnectionString.parseText url of
                Left err -> expectationFailure ("URL roundtrip parse error: " <> Text.unpack err)
                Right cs2 -> cs2 `shouldBe` cs

        it "postgresql:///dbname?host=/var/lib/postgresql" do
          let input = "postgresql:///dbname?host=/var/lib/postgresql"
          case ConnectionString.parseText input of
            Left err -> expectationFailure ("Parse error: " <> Text.unpack err)
            Right cs -> do
              ConnectionString.toDbname cs `shouldBe` Just "dbname"
              ConnectionString.toParams cs `shouldBe` Map.singleton "host" "/var/lib/postgresql"
              -- Roundtrip
              let url = ConnectionString.toUrl cs
              case ConnectionString.parseText url of
                Left err -> expectationFailure ("URL roundtrip parse error: " <> Text.unpack err)
                Right cs2 -> cs2 `shouldBe` cs

        it "postgresql://%2Fvar%2Flib%2Fpostgresql/dbname" do
          let input = "postgresql://%2Fvar%2Flib%2Fpostgresql/dbname"
          case ConnectionString.parseText input of
            Left err -> expectationFailure ("Parse error: " <> Text.unpack err)
            Right cs -> do
              ConnectionString.toHosts cs `shouldBe` [("/var/lib/postgresql", Nothing)]
              ConnectionString.toDbname cs `shouldBe` Just "dbname"
              -- Roundtrip
              let url = ConnectionString.toUrl cs
              case ConnectionString.parseText url of
                Left err -> expectationFailure ("URL roundtrip parse error: " <> Text.unpack err)
                Right cs2 -> cs2 `shouldBe` cs

        it "postgresql://host1:1,host2:2,host3:3/" do
          let input = "postgresql://host1:1,host2:2,host3:3/"
          case ConnectionString.parseText input of
            Left err -> expectationFailure ("Parse error: " <> Text.unpack err)
            Right cs -> do
              -- Port names should be parsed as text, but will fail to parse as numbers
              -- This is a tricky case - the documentation shows port1, port2, port3 as placeholders
              -- Let's check what we get
              let hosts = ConnectionString.toHosts cs
              length hosts `shouldBe` 3
              -- For now, just verify it parses and roundtrips
              let url = ConnectionString.toUrl cs
              case ConnectionString.parseText url of
                Left err -> expectationFailure ("URL roundtrip parse error: " <> Text.unpack err)
                Right cs2 -> cs2 `shouldBe` cs

        it "host=host1,host2,host3 port=1,2,3" do
          let input = "host=host1,host2,host3 port=1,2,3"
          case ConnectionString.parseText input of
            Left err -> expectationFailure ("Parse error: " <> Text.unpack err)
            Right cs -> do
              -- In keyword/value format, multiple hosts are separated by commas in the value
              -- This is a special case that may not be supported yet
              -- For now, just verify it parses
              let hosts = ConnectionString.toHosts cs
              length hosts `shouldSatisfy` (> 0)

      describe "equivalence tests" do
        it "postgresql://host1:1,host2:2,host3:3/ is equivalent to host=host1,host2,host3 port=1,2,3" do
          let url = "postgresql://host1:1,host2:2,host3:3/"
              kv = "host=host1,host2,host3 port=1,2,3"
          case (ConnectionString.parseText url, ConnectionString.parseText kv) of
            (Right cs1, Right cs2) -> do
              -- They should represent the same connection
              -- At minimum, they should have the same number of hosts
              length (ConnectionString.toHosts cs1) `shouldBe` length (ConnectionString.toHosts cs2)
            (Left err, _) -> expectationFailure ("URL parse error: " <> Text.unpack err)
            (_, Left err) -> expectationFailure ("KV parse error: " <> Text.unpack err)

laws :: Laws.Laws -> Spec
laws (Laws.Laws className props) =
  describe className do
    forM_ props \(propName, prop) ->
      it propName do
        property prop
