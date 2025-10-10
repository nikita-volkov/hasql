module Hasql.Connection.Settings
  ( -- * Settings
    Settings,

    -- * Constructors
    connectionString,
    noPreparedStatements,
    host,
    hostAndPort,
    user,
    password,
    dbname,
    applicationName,
    other,
  )
where

import Data.Text qualified as Text
import Hasql.Connection.Config qualified as Config
import Platform.Prelude
import PostgresqlConnectionString qualified as ConnectionString

-- | Connection settings.
-- This is a monoid, so you can combine multiple settings using 'mappend' ('<>').
-- The rightmost setting takes precedence in case of conflicts.
newtype Settings
  = Settings ConnectionString.ConnectionString
  deriving newtype (Eq, IsString, Show, Semigroup, Monoid)

-- | This instance allows interfacing with the internal Config.Config type without affecting the public API or requiring complex module hierarchies.
instance Config.Constructs Settings where
  construct (Settings connectionString) =
    case ConnectionString.interceptParam "no_prepared_statements" connectionString of
      Just (value, connectionString) ->
        let noPreparedStatements = interpretTextAsBool value
         in pack connectionString noPreparedStatements
      Nothing -> pack connectionString False
    where
      interpretTextAsBool value = case Text.toLower value of
        "1" -> True
        "true" -> True
        "t" -> True
        "yes" -> True
        "y" -> True
        "on" -> True
        _ -> False

      pack connectionString noPreparedStatements =
        Config.Config
          { connectionString =
              let textUrl = ConnectionString.toUrl connectionString
               in encodeUtf8 textUrl,
            noPreparedStatements
          }

-- * Constructors

-- | Whether prepared statements are disabled.
--
-- 'False' by default.
--
-- When 'True', even the statements marked as preparable will be executed without preparation at the cost of reduced performance.
--
-- This is useful when dealing with proxying applications like @pgbouncer@, which may be incompatible with prepared statements.
-- Consult their docs or just provide this setting to stay on the safe side.
-- It should be noted that starting from version @1.21.0@ @pgbouncer@ now does provide support for prepared statements.
noPreparedStatements :: Bool -> Settings
noPreparedStatements =
  Settings . ConnectionString.param "no_prepared_statements" . bool "false" "true"

-- | Host domain name or IP-address.
--
-- To specify multiple alternate hosts, combine the produced settings via 'Monoid'.
host :: Text -> Settings
host host =
  Settings (ConnectionString.host host)

-- | Host domain name or IP-address.
--
-- To specify multiple alternate hosts, combine the produced settings via 'Monoid'.
hostAndPort :: Text -> Word16 -> Settings
hostAndPort host port =
  Settings (ConnectionString.hostAndPort host port)

-- | User name.
user :: Text -> Settings
user = Settings . ConnectionString.user

-- | Password.
password :: Text -> Settings
password = Settings . ConnectionString.password

-- | Database name.
dbname :: Text -> Settings
dbname = Settings . ConnectionString.dbname

-- | Application name.
applicationName :: Text -> Settings
applicationName = Settings . ConnectionString.param "application_name"

-- | Other param.
other :: Text -> Text -> Settings
other key = Settings . ConnectionString.param key

-- | Construct from a connection string in either URI or key-value format.
--
-- See the [PostgreSQL documentation](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING) for details on the format.
--
-- If the connection string is invalid, it will be treated as empty.
connectionString :: Text -> Settings
connectionString = Settings . either (const mempty) id . ConnectionString.parse
