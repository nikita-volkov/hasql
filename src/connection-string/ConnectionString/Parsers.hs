{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module ConnectionString.Parsers where

import ConnectionString.Types
import Data.CharSet qualified as CharSet
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import PercentEncoding qualified
import Platform.Prelude hiding (many, some, try)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type P = Parsec Void Text

getConnectionString :: P ConnectionString
getConnectionString =
  asum
    [ getUriConnectionString,
      getKeyValueConnectionString
    ]

getUriConnectionString :: P ConnectionString
getUriConnectionString = do
  asum
    [ try (string' "postgresql://"),
      try (string' "postgres://")
    ]
  asum
    [ do
        unqualifiedWord <- try getWord
        asum
          [ do
              try (char ':')
              -- We still don't know if this is user:password@ or host:port
              asum
                [ do
                    password <- try do
                      label "Password" getWord <* char '@'
                    -- Definitely user:password@ pattern
                    let user = unqualifiedWord
                    continueFromHostspec (Just user) (Just password) [],
                  do
                    port <- try do
                      p <- decimal <?> "Port"
                      -- Ensure we're followed by valid continuation (not more alphanumeric)
                      notFollowedBy alphaNumChar
                      return p
                    let host = unqualifiedWord
                    continueAfterHostspec Nothing Nothing [Host host (Just port)]
                ],
            do
              try (char '@')
              -- Definitely user@ pattern
              let user = unqualifiedWord
              continueFromHostspec (Just user) Nothing [],
            do
              -- Definitely host pattern
              let host = unqualifiedWord
              continueAfterHostspec Nothing Nothing [Host host Nothing]
          ],
      do
        try (char '/')
        continueFromDbname Nothing Nothing [],
      do
        try (char '?')
        continueFromParams Nothing Nothing [] Nothing,
      pure (ConnectionString Nothing Nothing [] Nothing Map.empty)
    ]

getKeyValueConnectionString :: P ConnectionString
getKeyValueConnectionString = do
  params <- getKeyValueParams
  -- Extract known connection parameters
  let user = Map.lookup "user" params
      password = Map.lookup "password" params
      dbname = Map.lookup "dbname" params
      hostText = Map.lookup "host" params
      portText = Map.lookup "port" params
      -- Remove extracted params from the map
      remainingParams =
        ( Map.delete "user"
            . Map.delete "password"
            . Map.delete "dbname"
            . Map.delete "host"
            . Map.delete "port"
        )
          params

      -- Parse hosts if present - handle comma-separated hosts and ports
      hosts = case hostText of
        Nothing -> []
        Just h ->
          let hostList = Text.splitOn "," h
              portList = maybe [] (Text.splitOn ",") portText
              -- Pair up hosts with ports, padding with Nothing if needed
              pairs = zipWith (\host mPort -> (host, mPort)) hostList (map Just portList ++ repeat Nothing)
           in map (\(host, mPortText) -> Host host (mPortText >>= parsePort)) pairs

  pure (ConnectionString user password hosts dbname remainingParams)
  where
    parsePort :: Text -> Maybe Word16
    parsePort t = case reads (Text.unpack t) of
      [(n, "")] -> Just n
      _ -> Nothing

getKeyValueParams :: P (Map.Map Text Text)
getKeyValueParams = do
  firstParam <- getKeyValueParam
  restParams <- many do
    space1
    getKeyValueParam
  pure (Map.fromList (firstParam : restParams))

getKeyValueParam :: P (Text, Text)
getKeyValueParam = do
  key <- some (satisfy (\c -> c /= '=' && c /= ' '))
  char '='
  value <- getKeyValueParamValue
  pure (fromString key, fromString value)

getKeyValueParamValue :: P String
getKeyValueParamValue =
  asum
    [ do
        -- Quoted value
        char '\''
        chars <- many do
          asum
            [ do
                -- Escaped quote
                try (string "\\'")
                pure '\'',
              do
                -- Escaped backslash
                try (string "\\\\")
                pure '\\',
              satisfy (/= '\'')
            ]
        char '\''
        pure chars,
      -- Unquoted value
      some (satisfy (\c -> c /= ' ' && c /= '\n'))
    ]

getWord :: P Text
getWord = PercentEncoding.parser (flip CharSet.member controlCharset)
  where
    controlCharset = CharSet.fromList ":@?/=&,"

getParamValue :: P Text
getParamValue = PercentEncoding.parser (flip CharSet.member paramControlCharset)
  where
    paramControlCharset = CharSet.fromList "&"

continueAfterHostspec :: Maybe Text -> Maybe Text -> [Host] -> P ConnectionString
continueAfterHostspec user password hosts = do
  asum
    [ do
        try (char '/')
        continueFromDbname user password hosts,
      do
        try (char '?')
        continueFromParams user password hosts Nothing,
      do
        try (char ',')
        continueFromHostspec user password hosts,
      pure (ConnectionString user password hosts Nothing Map.empty)
    ]

continueFromHostspec :: Maybe Text -> Maybe Text -> [Host] -> P ConnectionString
continueFromHostspec user password hosts = do
  -- Check if there's actually a host to parse
  asum
    [ do
        try (char '/')
        continueFromDbname user password hosts,
      do
        try (char '?')
        continueFromParams user password hosts Nothing,
      do
        -- Parse first host
        firstHost <- getHost

        -- Parse additional hosts (comma-separated)
        moreHosts <- many do
          try (char ',')
          getHost

        let allHosts = hosts <> [firstHost] <> moreHosts

        asum
          [ do
              try (char '/')
              continueFromDbname user password allHosts,
            do
              try (char '?')
              continueFromParams user password allHosts Nothing,
            pure (ConnectionString user password allHosts Nothing Map.empty)
          ],
      pure (ConnectionString user password hosts Nothing Map.empty)
    ]

getHost :: P Host
getHost = do
  host <- getWord
  port <- optional do
    -- Try to parse as numeric port
    try do
      char ':'
    label "Port" decimal <* notFollowedBy alphaNumChar
  pure (Host host port)

continueFromDbname :: Maybe Text -> Maybe Text -> [Host] -> P ConnectionString
continueFromDbname user password hosts = do
  dbname <- optional getWord
  asum
    [ do
        try (char '?')
        continueFromParams user password hosts dbname,
      pure (ConnectionString user password hosts dbname Map.empty)
    ]

continueFromParams :: Maybe Text -> Maybe Text -> [Host] -> Maybe Text -> P ConnectionString
continueFromParams user password hosts dbname = do
  params <- getParams
  pure (ConnectionString user password hosts dbname params)

getParams :: P (Map.Map Text Text)
getParams = do
  firstParam <- getParam
  restParams <- many do
    char '&'
    getParam
  pure (Map.fromList (firstParam : restParams))

getParam :: P (Text, Text)
getParam = do
  key <- getWord
  char '='
  value <- getParamValue
  pure (key, value)
