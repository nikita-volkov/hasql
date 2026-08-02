module Hasql.Engine.Structures.StatementCache
  ( -- * Pure registry operations
    StatementCache,
    empty,
    lookup,
    insert,
    reset,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Hasql.Platform.Prelude hiding (empty, insert, lookup, reset)

-- | Pure registry state containing the hash map and counter
data StatementCache = StatementCache (HashMap LocalKey ByteString) Word
  deriving stock (Show, Eq)

-- | Create an empty registry state
{-# INLINEABLE empty #-}
empty :: StatementCache
empty = StatementCache HashMap.empty 0

-- | Pure lookup operation
{-# INLINEABLE lookup #-}
lookup :: ByteString -> [Word32] -> StatementCache -> Maybe ByteString
lookup sql oids (StatementCache hashMap _) = HashMap.lookup localKey hashMap
  where
    localKey = LocalKey sql oids

-- | Pure insert operation that returns new state and the generated remote key
{-# INLINEABLE insert #-}
insert :: ByteString -> [Word32] -> StatementCache -> (ByteString, StatementCache)
insert sql oids (StatementCache hashMap counter) = (remoteKey, newState)
  where
    remoteKey = fromString $ show $ newCounter
    newHashMap = HashMap.insert localKey remoteKey hashMap
    newCounter = counter + 1
    newState = StatementCache newHashMap newCounter
    localKey = LocalKey sql oids

-- | Pure reset operation
{-# INLINEABLE reset #-}
reset :: StatementCache -> StatementCache
reset _ = StatementCache HashMap.empty 0

-- |
-- Local statement key.
data LocalKey
  = LocalKey ByteString [Word32]
  deriving (Show, Eq)

instance Hashable LocalKey where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt (LocalKey template oids) =
    hashWithSalt (hashWithSalt salt template) oids
