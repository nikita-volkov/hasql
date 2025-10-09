{-# OPTIONS_GHC -Wno-orphans #-}

module ConnectionString
  ( ConnectionString,
    parseText,
    parserOf,

    -- * Accessors
    toHosts,
    toUser,
    toPassword,
    toDbname,
    toParams,
    toUrl,
    toKeyValueString,

    -- * Constructors
    hostAndPort,
    user,
    password,
    dbname,
    param,
  )
where

import ConnectionString.Parsers qualified as Parsers
import ConnectionString.Types
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import PercentEncoding qualified
import Platform.Prelude
import Text.Megaparsec qualified as Megaparsec
import TextBuilder qualified

instance IsString ConnectionString where
  fromString =
    either fromError id . parseText . fromString
    where
      fromError = const mempty

instance Show ConnectionString where
  showsPrec d = showsPrec d . toUrl

toHosts :: ConnectionString -> [(Text, Maybe Word16)]
toHosts (ConnectionString _ _ hostspec _ _) =
  map (\(Host host port) -> (host, port)) hostspec

toUser :: ConnectionString -> Maybe Text
toUser (ConnectionString user _ _ _ _) = user

toPassword :: ConnectionString -> Maybe Text
toPassword (ConnectionString _ password _ _ _) = password

toDbname :: ConnectionString -> Maybe Text
toDbname (ConnectionString _ _ _ dbname _) = dbname

toParams :: ConnectionString -> Map.Map Text Text
toParams (ConnectionString _ _ _ _ paramspec) = paramspec

toUrl :: ConnectionString -> Text
toUrl = TextBuilder.toText . renderConnectionString
  where
    renderConnectionString (ConnectionString user password hostspec dbname paramspec) =
      -- postgresql://[userspec@][hostspec][/dbname][?paramspec]
      mconcat
        [ "postgresql://",
          renderUserspec user password,
          TextBuilder.intercalateMap "," renderHost hostspec,
          foldMap (mappend "/" . PercentEncoding.encodeText) dbname,
          renderParamspec paramspec
        ]

    renderUserspec user password =
      case user of
        Nothing -> mempty
        Just user ->
          mconcat
            [ PercentEncoding.encodeText user,
              foldMap (mappend ":" . PercentEncoding.encodeText) password,
              "@"
            ]

    renderHost (Host host port) =
      mconcat
        [ PercentEncoding.encodeText host,
          foldMap renderPort port
        ]

    renderPort port =
      mconcat
        [ ":",
          TextBuilder.decimal port
        ]

    renderParamspec paramspec =
      case Map.toList paramspec of
        [] -> mempty
        list ->
          mconcat
            [ "?",
              TextBuilder.intercalateMap "&" renderParam list
            ]

    renderParam (key, value) =
      mconcat
        [ PercentEncoding.encodeText key,
          "=",
          PercentEncoding.encodeText value
        ]

-- | Convert a connection string to the PostgreSQL keyword/value format.
--
-- The keyword/value format is a space-separated list of key=value pairs.
-- Values containing spaces, quotes, backslashes, or equals signs are automatically
-- quoted with single quotes, and backslashes and single quotes within values are
-- escaped with backslashes.
--
-- Note: Only the first host from the hostspec is included, as the keyword/value
-- format does not support multiple hosts in the same way as the URI format.
--
-- Examples:
--
-- >>> toKeyValueString (mconcat [hostAndPort "localhost" (Just 5432), user "postgres"])
-- "host=localhost port=5432 user=postgres"
--
-- >>> toKeyValueString (password "secret pass")
-- "password='secret pass'"
--
-- >>> toKeyValueString (password "it's a secret")
-- "password='it\\'s a secret'"
toKeyValueString :: ConnectionString -> Text
toKeyValueString (ConnectionString user password hostspec dbname paramspec) =
  (TextBuilder.toText . TextBuilder.intercalateMap " " id)
    ( catMaybes
        [ fmap (\h -> renderKeyValue "host" (renderHostForKeyValue h)) (listToMaybe hostspec),
          fmap (\p -> renderKeyValue "port" (TextBuilder.decimal p)) (listToMaybe hostspec >>= \(Host _ p) -> p),
          fmap (renderKeyValue "user" . TextBuilder.text) user,
          fmap (renderKeyValue "password" . TextBuilder.text) password,
          fmap (renderKeyValue "dbname" . TextBuilder.text) dbname
        ]
        <> map (\(k, v) -> renderKeyValue k (TextBuilder.text v)) (Map.toList paramspec)
    )
  where
    renderHostForKeyValue (Host host _) = TextBuilder.text host

    renderKeyValue key value =
      mconcat
        [ TextBuilder.text key,
          "=",
          escapeValue value
        ]

    -- Escape values according to the keyword/value format rules
    escapeValue :: TextBuilder -> TextBuilder
    escapeValue valueBuilder =
      let value = TextBuilder.toText valueBuilder
       in if needsQuoting value
            then mconcat ["'", TextBuilder.text (escapeForQuoted value), "'"]
            else TextBuilder.text value

    -- Check if a value needs quoting
    needsQuoting :: Text -> Bool
    needsQuoting value =
      Text.null value
        || Text.any (\c -> c == ' ' || c == '\'' || c == '\\' || c == '=') value

    -- Escape backslashes and single quotes for quoted values
    escapeForQuoted :: Text -> Text
    escapeForQuoted = Text.concatMap escapeChar
      where
        escapeChar '\\' = "\\\\"
        escapeChar '\'' = "\\'"
        escapeChar c = Text.singleton c

-- * Parsing

parseText :: Text -> Either Text ConnectionString
parseText input =
  Megaparsec.parse parserOf "" input
    & first (fromString . Megaparsec.errorBundlePretty)

parserOf :: Megaparsec.Parsec Void Text ConnectionString
parserOf = Parsers.getConnectionString

-- * Constructors

hostAndPort :: Text -> Maybe Word16 -> ConnectionString
hostAndPort host port =
  ConnectionString
    Nothing
    Nothing
    [Host host port]
    Nothing
    Map.empty

user :: Text -> ConnectionString
user username =
  ConnectionString
    (Just username)
    Nothing
    []
    Nothing
    Map.empty

password :: Text -> ConnectionString
password pwd =
  ConnectionString
    Nothing
    (Just pwd)
    []
    Nothing
    Map.empty

dbname :: Text -> ConnectionString
dbname db =
  ConnectionString
    Nothing
    Nothing
    []
    (Just db)
    Map.empty

param :: Text -> Text -> ConnectionString
param key value =
  ConnectionString
    Nothing
    Nothing
    []
    Nothing
    (Map.singleton key value)
