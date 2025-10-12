module Core.Structures.StatementCache
  ( -- * Pure registry operations
    StatementCache,
    empty,
    lookup,
    insert,
    reset,

    -- * Key type
    LocalKey (..),
  )
where

import Data.HashMap.Strict qualified as HashMap
import Platform.Prelude hiding (empty, insert, lookup, reset)
import Pq qualified

-- | Pure registry state containing the hash map and counter
data StatementCache = StatementCache (HashMap.HashMap LocalKey ByteString) Word
  deriving stock (Show, Eq)

-- | Create an empty registry state
{-# INLINEABLE empty #-}
empty :: StatementCache
empty = StatementCache HashMap.empty 0

-- | Pure lookup operation
{-# INLINEABLE lookup #-}
lookup :: LocalKey -> StatementCache -> Maybe ByteString
lookup localKey (StatementCache hashMap _) = HashMap.lookup localKey hashMap

-- | Pure insert operation that returns new state and the generated remote key
{-# INLINEABLE insert #-}
insert :: LocalKey -> StatementCache -> (ByteString, StatementCache)
insert localKey (StatementCache hashMap counter) = (remoteKey, newState)
  where
    remoteKey = fromString $ show $ newCounter
    newHashMap = HashMap.insert localKey remoteKey hashMap
    newCounter = succ counter
    newState = StatementCache newHashMap newCounter

-- | Pure reset operation
{-# INLINEABLE reset #-}
reset :: StatementCache -> StatementCache
reset _ = StatementCache HashMap.empty 0

-- |
-- Local statement key.
data LocalKey
  = LocalKey ByteString [Pq.Oid]
  deriving (Show, Eq)

instance Hashable LocalKey where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt (LocalKey template _) =
    hashWithSalt salt template
