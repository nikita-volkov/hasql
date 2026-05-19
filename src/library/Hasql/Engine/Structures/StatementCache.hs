module Hasql.Engine.Structures.StatementCache
  ( -- * Pure registry operations
    StatementCache,
    empty,
    lookup,
    insert,
    reset,
    revertTo,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Hasql.Platform.Prelude hiding (empty, insert, lookup, reset)
import Hasql.Pq qualified as Pq

-- | Pure registry state containing the hash map and counter
data StatementCache = StatementCache (HashMap LocalKey ByteString) Word
  deriving stock (Show, Eq)

-- | Create an empty registry state
{-# INLINEABLE empty #-}
empty :: StatementCache
empty = StatementCache HashMap.empty 0

-- | Pure lookup operation
{-# INLINEABLE lookup #-}
lookup :: ByteString -> [Pq.Oid] -> StatementCache -> Maybe ByteString
lookup sql oids (StatementCache hashMap _) = HashMap.lookup localKey hashMap
  where
    localKey = LocalKey sql oids

-- | Pure insert operation that returns new state and the generated remote key
{-# INLINEABLE insert #-}
insert :: ByteString -> [Pq.Oid] -> StatementCache -> (ByteString, StatementCache)
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

-- | Revert the entries to those of the provided cache, preserving the counter of the receiver.
--
-- This is useful when a pipeline fails and we want to discard tentative entries
-- (which might not have been successfully prepared on the server)
-- while keeping the counter advanced to avoid name collisions with orphaned server-side statements.
{-# INLINEABLE revertTo #-}
revertTo :: StatementCache -> StatementCache -> StatementCache
revertTo (StatementCache oldHashMap _) (StatementCache _ newCounter) =
  StatementCache oldHashMap newCounter

-- |
-- Local statement key.
data LocalKey
  = LocalKey ByteString [Pq.Oid]
  deriving (Show, Eq)

instance Hashable LocalKey where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt (LocalKey template oids) =
    hashWithSalt (hashWithSalt salt template) (fmap Pq.oidToWord32 oids)
