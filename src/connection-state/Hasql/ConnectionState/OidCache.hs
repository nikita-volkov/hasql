module Hasql.ConnectionState.OidCache
  ( OidCache,

    -- * Accessors
    lookupTypeInfo,
    toResolver,

    -- * Constructors
    fromHashMap,
    empty,
    selectUnknownNames,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Hasql.CodecsCore.QualifiedTypeName (QualifiedTypeName)
import Hasql.CodecsCore.QualifiedTypeName qualified as QualifiedTypeName
import Hasql.CodecsCore.TypeInfo qualified as TypeInfo
import Hasql.Platform.Prelude hiding (empty, insert, lookup, reset)

-- | Pure registry state containing the hash map and counter
newtype OidCache
  = OidCache
      -- | By name of the type.
      --
      -- > scalar name -> TypeInfo (scalar OID, array OID)
      (HashMap QualifiedTypeName TypeInfo.TypeInfo)
  deriving stock (Show, Eq)

instance Semigroup OidCache where
  OidCache byNameL <> OidCache byNameR =
    OidCache (HashMap.union byNameR byNameL)

instance Monoid OidCache where
  mempty = OidCache mempty

{-# INLINEABLE empty #-}
empty :: OidCache
empty =
  OidCache HashMap.empty

-- | Having a set of required type names, select those that are not present in the cache.
{-# INLINE selectUnknownNames #-}
selectUnknownNames :: HashSet QualifiedTypeName -> OidCache -> HashSet QualifiedTypeName
selectUnknownNames keys (OidCache byName) =
  HashSet.filter (\key -> not (HashMap.member key byName)) keys

{-# INLINE fromHashMap #-}
fromHashMap :: HashMap QualifiedTypeName TypeInfo.TypeInfo -> OidCache
fromHashMap byName = OidCache byName

-- * Accessors

{-# INLINE lookupTypeInfo #-}
lookupTypeInfo :: QualifiedTypeName -> OidCache -> Maybe TypeInfo.TypeInfo
lookupTypeInfo name (OidCache byName) =
  HashMap.lookup name byName

-- | Resolution function for a name against the cache, falling back to 'TypeInfo.invalid' on a miss.
{-# INLINE toResolver #-}
toResolver :: OidCache -> QualifiedTypeName -> TypeInfo.TypeInfo
toResolver oidCache name =
  lookupTypeInfo name oidCache & fromMaybe TypeInfo.invalid
