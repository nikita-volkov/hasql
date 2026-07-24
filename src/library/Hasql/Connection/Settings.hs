module Hasql.Connection.Settings
  ( -- * Settings
    Settings,

    -- * Constructors
    host,
    hostAndPort,
    user,
    password,
    dbname,
    applicationName,
    other,
    statementCacheSize,
    prepareThreshold,
    connectionString,
  )
where

import Data.Text.Read qualified as TextRead
import Hasql.Connection.Config qualified as Config
import Hasql.Platform.Prelude
import PostgresqlConnectionString qualified as ConnectionString

-- | Connection settings.
--
-- This is a monoid, so you can combine multiple settings using 'mappend' ('<>').
-- The rightmost setting takes precedence in case of conflicts.
--
-- With OverloadedStrings, you can declare settings using connection strings directly.
--
-- For example, using a key-value format:
--
-- >>> "host=localhost port=5432 user=myuser dbname=mydb" :: Settings
-- "postgresql://myuser@localhost:5432/mydb"
--
-- Or using a URI format:
--
-- >>> "postgresql://myuser@localhost:5432/mydb" :: Settings
-- "postgresql://myuser@localhost:5432/mydb"
--
-- You can achieve the same effect by constructing from a 'Text' value:
--
-- >>> connectionString "host=localhost port=5432 user=myuser dbname=mydb"
-- "postgresql://myuser@localhost:5432/mydb"
--
-- Or use the provided constructors for better type safety and clarity:
--
-- >>> hostAndPort "localhost" 5432 <> user "myuser" <> dbname "mydb"
-- "postgresql://myuser@localhost:5432/mydb"
newtype Settings
  = Settings ConnectionString.ConnectionString
  deriving newtype (Eq, IsString, Show, Semigroup, Monoid)

-- | This instance allows interfacing with the internal Config.Config type without affecting the public API or requiring complex module hierarchies.
instance Config.Constructs Settings where
  construct (Settings connectionString) =
    Config.Config
      { connectionString = encodeUtf8 (ConnectionString.toUrl remainder),
        statementCacheSize = fromMaybe Config.defaultStatementCacheSize interceptedCacheSize,
        prepareThreshold = fromMaybe Config.defaultPrepareThreshold interceptedThreshold
      }
    where
      (interceptedCacheSize, afterCacheSize) =
        interceptInt statementCacheSizeParam connectionString
      (interceptedThreshold, remainder) =
        interceptInt prepareThresholdParam afterCacheSize

      -- An unparseable value is treated as absent, consistently with how an
      -- invalid connection string is treated as empty.
      interceptInt name string =
        case ConnectionString.interceptParam name string of
          Nothing -> (Nothing, string)
          Just (value, remainder) ->
            case TextRead.signed TextRead.decimal value of
              Right (int, "") -> (Just int, remainder)
              _ -> (Nothing, remainder)

statementCacheSizeParam :: Text
statementCacheSizeParam = "statement_cache_size"

prepareThresholdParam :: Text
prepareThresholdParam = "prepare_threshold"

-- * Constructors

-- | Maximum amount of statements kept prepared on a connection.
--
-- @1024@ by default. Negative values are treated as @0@.
--
-- The driver prepares statements on its own, based on how often they are
-- actually executed, and deallocates the least recently used one when this
-- limit is reached. The limit is per connection, so the worst-case amount of
-- plans held by the server is this value multiplied by the size of your pool.
--
-- @0@ disables preparation completely. Use it when talking to a proxy that
-- cannot handle prepared statements, such as @pgbouncer@ before version
-- @1.21.0@.
statementCacheSize :: Int -> Settings
statementCacheSize =
  Settings . ConnectionString.param statementCacheSizeParam . fromString . show

-- | Amount of executions of a statement on a connection after which it gets
-- prepared.
--
-- @2@ by default. Values below @1@ are treated as @1@, which means preparing
-- on first use.
--
-- Raising it makes the driver more conservative about spending a @PARSE@ on
-- statements that may never be executed again, at the cost of one extra
-- unprepared execution for the statements that are. It cannot disable
-- preparation — use 'statementCacheSize' @0@ for that.
prepareThreshold :: Int -> Settings
prepareThreshold =
  Settings . ConnectionString.param prepareThresholdParam . fromString . show

-- | Host domain name or IP-address.
--
-- To specify multiple alternate hosts, combine the produced settings via 'Monoid'.
host :: Text -> Settings
host host =
  Settings (ConnectionString.host host)

-- | Host domain name or IP-address and port.
--
-- Specifying a port without a host is not allowed due to how PostgreSQL handles connection strings.
--
-- This function creates a single host-port pair. To specify multiple alternate hosts, combine the results of multiple 'hostAndPort' calls using the 'Monoid' instance.
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
