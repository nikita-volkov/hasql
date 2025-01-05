module Hasql.ConnectionString where

import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Hasql.Prelude

-- |
-- All settings encoded in a single byte-string according to
-- <http://www.postgresql.org/docs/9.4/static/libpq-connect.html#LIBPQ-CONNSTRING the PostgreSQL format>.
type ConnectionString =
  ByteString

-- |
-- Encode a host, a port, a user, a password and a database into the PostgreSQL settings byte-string.
{-# INLINE connectionString #-}
connectionString :: ByteString -> Word16 -> ByteString -> ByteString -> ByteString -> ConnectionString
connectionString host port user password database =
  BL.toStrict
    $ BB.toLazyByteString
    $ mconcat
    $ intersperse (BB.char7 ' ')
    $ catMaybes
    $ [ mappend (BB.string7 "host=")
          . BB.byteString
          <$> mfilter (not . B.null) (pure host),
        mappend (BB.string7 "port=")
          . BB.word16Dec
          <$> mfilter (/= 0) (pure port),
        mappend (BB.string7 "user=")
          . BB.byteString
          <$> mfilter (not . B.null) (pure user),
        mappend (BB.string7 "password=")
          . BB.byteString
          <$> mfilter (not . B.null) (pure password),
        mappend (BB.string7 "dbname=")
          . BB.byteString
          <$> mfilter (not . B.null) (pure database)
      ]
