module Hasql.Connection.Settings
  ( -- * Settings
    Settings,

    -- * Constructors
    connectionString,
    noPreparedStatements,
    hostAndPort,
    user,
    password,
    dbname,
    applicationName,
    other,
  )
where

import ConnectionString qualified
import Data.Text qualified as Text
import Platform.Prelude

-- | Connection settings.
-- This is a monoid, so you can combine multiple settings using 'mappend' ('<>').
-- The rightmost setting takes precedence in case of conflicts.
data Settings
  = Settings
      -- | Connection string.
      ConnectionString.ConnectionString
      -- | Disable the use of prepared statements.
      Bool
  deriving stock (Eq)

-- | Combines settings, with the rightmost taking precedence in case of conflicts.
--
-- In case of host addresses, they get combined as alternatives.
instance Semigroup Settings where
  Settings connectionString1 _noPreparedStatements1 <> Settings connectionString2 noPreparedStatements2 =
    Settings
      (connectionString1 <> connectionString2)
      noPreparedStatements2

-- | Provides default settings.
instance Monoid Settings where
  mempty = Settings mempty False

-- | Renders as a string literal with a URL.
instance Show Settings where
  showsPrec d = showsPrec d . toConnectionString

-- | Constructs from a connection string in either URI or key-value format.
instance IsString Settings where
  fromString = fromConnectionString . fromString

toConnectionString :: Settings -> ConnectionString.ConnectionString
toConnectionString (Settings connectionString noPreparedStatements) =
  mconcat
    [ connectionString,
      if noPreparedStatements
        then ConnectionString.param "no_prepared_statements" "true"
        else mempty
    ]

fromConnectionString :: ConnectionString.ConnectionString -> Settings
fromConnectionString connectionString =
  case ConnectionString.interceptParam "no_prepared_statements" connectionString of
    Just (value, connectionString') ->
      let noPreparedStatements = case Text.toLower value of
            "1" -> True
            "true" -> True
            "t" -> True
            "yes" -> True
            "y" -> True
            "on" -> True
            _ -> False
       in Settings connectionString' noPreparedStatements
    Nothing -> Settings connectionString False

-- * Constructors

-- | Disable the use of prepared statements.
noPreparedStatements :: Bool -> Settings
noPreparedStatements = Settings mempty

-- | Host domain name or IP-address.
--
-- To specify multiple alternate hosts, combine the produced settings via 'Monoid'.
hostAndPort :: Text -> Maybe Word16 -> Settings
hostAndPort host port =
  fromConnectionString (ConnectionString.hostAndPort host port)

-- | User name.
user :: Text -> Settings
user = fromConnectionString . ConnectionString.user

-- | Password.
password :: Text -> Settings
password = fromConnectionString . ConnectionString.password

-- | Database name.
dbname :: Text -> Settings
dbname = fromConnectionString . ConnectionString.dbname

-- | Application name.
applicationName :: Text -> Settings
applicationName = fromConnectionString . ConnectionString.param "application_name"

-- | Other param.
other :: Text -> Text -> Settings
other key = fromConnectionString . ConnectionString.param key

-- | Construct from a connection string in either URI or key-value format.
--
-- See the [PostgreSQL documentation](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING) for details on the format.
--
-- If the connection string is invalid, it will be treated as empty.
connectionString :: Text -> Settings
connectionString = fromConnectionString . either (const mempty) id . ConnectionString.parseText
