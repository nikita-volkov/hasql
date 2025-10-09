{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module ConnectionString where

import Data.Map.Strict qualified as Map
import PercentEncoding qualified
import Platform.Prelude
import TextBuilder qualified

-- |
-- https://www.postgresql.org/docs/17/libpq-connect.html#LIBPQ-CONNSTRING-URIS
--
-- Examples:
--
-- > postgresql://
-- > postgresql://localhost
-- > postgresql://localhost:5433
-- > postgresql://localhost/mydb
-- > postgresql://user@localhost
-- > postgresql://user:secret@localhost
-- > postgresql://other@localhost/otherdb?connect_timeout=10&application_name=myapp
-- > postgresql://host1:123,host2:456/somedb?target_session_attrs=any&application_name=myapp
data ConnectionString
  = ConnectionString
      -- | User.
      (Maybe Text)
      -- | Password.
      (Maybe Text)
      -- | Host specification.
      [Host]
      -- | Database name.
      (Maybe Text)
      -- | Key-value parameters.
      (Map.Map Text Text)

data Host
  = Host
      -- | Host domain name or IP-address.
      Text
      -- | Port number.
      Word16

instance Semigroup ConnectionString where
  ConnectionString user1 password1 hosts1 dbname1 params1 <> ConnectionString user2 password2 hosts2 dbname2 params2 =
    ConnectionString
      (user1 <|> user2)
      (password1 <|> password2)
      (hosts1 <> hosts2)
      (dbname1 <|> dbname2)
      (Map.union params2 params1)

instance Monoid ConnectionString where
  mempty = ConnectionString Nothing Nothing [] Nothing Map.empty

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
          ":",
          TextBuilder.decimal port
        ]

    renderParamspec paramspec =
      case Map.toList paramspec of
        [] -> mempty
        list -> mconcat ["?", TextBuilder.intercalateMap "&" renderParam list]

    renderParam (key, value) =
      mconcat
        [ PercentEncoding.encodeText key,
          "=",
          PercentEncoding.encodeText value
        ]
