module Hasql.ConnectionState.StatementCache
  ( StatementCache,
    empty,
    lookup,
    insert,
    reset,
  )
where

import ByteString.StrictBuilder qualified
import Crypto.Hash.SHA256 qualified
import Data.ByteString qualified as ByteString
import Data.ByteString.Base16 qualified as Base16
import Data.HashMap.Strict qualified as HashMap
import Hasql.Platform.Prelude hiding (empty, insert, lookup, reset)

-- | Pure registry state mapping local statement keys to their deterministic remote names
newtype StatementCache = StatementCache (HashMap LocalKey ByteString)
  deriving stock (Show, Eq)

-- | Create an empty registry state
{-# INLINEABLE empty #-}
empty :: StatementCache
empty = StatementCache HashMap.empty

-- | Pure lookup operation
{-# INLINEABLE lookup #-}
lookup :: ByteString -> [Word32] -> StatementCache -> Maybe ByteString
lookup sql oids (StatementCache hashMap) = HashMap.lookup (LocalKey sql oids) hashMap

-- | Pure insert operation that returns new state and the generated remote key
{-# INLINEABLE insert #-}
insert :: ByteString -> [Word32] -> StatementCache -> (ByteString, StatementCache)
insert sql oids (StatementCache hashMap) = (remoteKey, newState)
  where
    localKey = LocalKey sql oids
    remoteKey =
      "hasql_" <> ByteString.take 57 (Base16.encode (Crypto.Hash.SHA256.hash hashInput))
      where
        hashInput =
          ByteString.StrictBuilder.builderBytes
            ( mconcat
                [ ByteString.StrictBuilder.word64BE (fromIntegral (ByteString.length sql)),
                  ByteString.StrictBuilder.bytes sql,
                  ByteString.StrictBuilder.word64BE (fromIntegral (length oids)),
                  foldMap ByteString.StrictBuilder.word32BE oids
                ]
            )
    newState = StatementCache (HashMap.insert localKey remoteKey hashMap)

-- | Pure reset operation
{-# INLINEABLE reset #-}
reset :: StatementCache -> StatementCache
reset _ = StatementCache HashMap.empty

-- |
-- Local statement key.
data LocalKey
  = LocalKey ByteString [Word32]
  deriving (Show, Eq)

instance Hashable LocalKey where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt (LocalKey template oids) =
    hashWithSalt (hashWithSalt salt template) oids
