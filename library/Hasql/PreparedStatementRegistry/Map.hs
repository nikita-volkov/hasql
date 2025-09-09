module Hasql.PreparedStatementRegistry.Map
  ( -- * Pure registry operations
    RegistryState,
    empty,
    lookup,
    insert,
    reset,
    -- * Key type
    LocalKey (..),
  )
where

import ByteString.StrictBuilder qualified as B
import Data.HashMap.Strict qualified as HM
import Hasql.LibPq14 qualified as Pq
import Hasql.Prelude hiding (empty, insert, lookup, reset)

-- | Pure registry state containing the hash map and counter
type RegistryState = (HM.HashMap LocalKey ByteString, Word)

-- | Create an empty registry state
{-# INLINEABLE empty #-}
empty :: RegistryState
empty = (HM.empty, 0)

-- | Pure lookup operation
{-# INLINEABLE lookup #-}
lookup :: LocalKey -> RegistryState -> Maybe ByteString
lookup localKey (hashMap, _) = HM.lookup localKey hashMap

-- | Pure insert operation that returns new state and the generated remote key
{-# INLINEABLE insert #-}
insert :: LocalKey -> RegistryState -> (ByteString, RegistryState)
insert localKey (hashMap, counter) = (remoteKey, newState)
  where
    remoteKey = B.builderBytes . B.asciiIntegral $ counter
    newHashMap = HM.insert localKey remoteKey hashMap
    newCounter = succ counter
    newState = (newHashMap, newCounter)

-- | Pure reset operation
{-# INLINEABLE reset #-}
reset :: RegistryState -> RegistryState
reset _ = (HM.empty, 0)

-- |
-- Local statement key.
data LocalKey
  = LocalKey !ByteString ![Pq.Oid]
  deriving (Show, Eq)

instance Hashable LocalKey where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt (LocalKey template _) =
    hashWithSalt salt template