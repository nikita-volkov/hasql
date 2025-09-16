module Core.Structures.OIDCache
  ( -- * Pure cache operations
    OIDCache,
    empty,
    lookup,
    insert,
    reset,

    -- * Type info
    TypeInfo (..),
  )
where

import Data.HashMap.Strict qualified as HashMap
import Platform.Prelude hiding (empty, insert, lookup, reset)
import Pq qualified

-- | Information about a PostgreSQL type
data TypeInfo = TypeInfo
  { typeOID :: Pq.Oid,
    arrayOID :: Maybe Pq.Oid
  }
  deriving stock (Show, Eq)

-- | Pure cache containing mapping from type names to OIDs
newtype OIDCache = OIDCache (HashMap.HashMap Text TypeInfo)
  deriving stock (Show, Eq)

-- | Create an empty cache
{-# INLINEABLE empty #-}
empty :: OIDCache
empty = OIDCache HashMap.empty

-- | Pure lookup operation
{-# INLINEABLE lookup #-}
lookup :: Text -> OIDCache -> Maybe TypeInfo
lookup typeName (OIDCache hashMap) = HashMap.lookup typeName hashMap

-- | Pure insert operation
{-# INLINEABLE insert #-}
insert :: Text -> TypeInfo -> OIDCache -> OIDCache
insert typeName typeInfo (OIDCache hashMap) =
  OIDCache (HashMap.insert typeName typeInfo hashMap)

-- | Pure reset operation
{-# INLINEABLE reset #-}
reset :: OIDCache -> OIDCache
reset _ = OIDCache HashMap.empty