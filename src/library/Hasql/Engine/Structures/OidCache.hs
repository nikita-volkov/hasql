module Hasql.Engine.Structures.OidCache
  ( OidCache,

    -- * Accessors
    toHashMap,
    lookupScalar,
    lookupArray,

    -- * Constructors
    fromHashMap,
    empty,
    selectUnknownNames,
    insertScalar,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Hasql.Kernel qualified as Kernel
import Hasql.Kernel.QualifiedTypeName qualified as Kernel.QualifiedTypeName
import Hasql.Kernel.TypeInfo qualified as Kernel.TypeInfo
import Hasql.Platform.Prelude hiding (empty, insert, lookup, reset)

-- | Pure registry state containing the hash map and counter
newtype OidCache
  = OidCache
      -- | By name of the type.
      --
      -- > scalar name -> TypeInfo (scalar OID, array OID)
      (HashMap Kernel.QualifiedTypeName Kernel.TypeInfo.TypeInfo)
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
selectUnknownNames :: HashSet Kernel.QualifiedTypeName -> OidCache -> HashSet Kernel.QualifiedTypeName
selectUnknownNames keys (OidCache byName) =
  HashSet.filter (\key -> not (HashMap.member key byName)) keys

insertScalar :: Maybe Text -> Text -> Word32 -> Word32 -> OidCache -> OidCache
insertScalar schema name scalar array (OidCache byName) =
  OidCache (HashMap.insert (Kernel.QualifiedTypeName.QualifiedTypeName schema name) (Kernel.TypeInfo.TypeInfo scalar array) byName)

{-# INLINE fromHashMap #-}
fromHashMap :: HashMap Kernel.QualifiedTypeName Kernel.TypeInfo.TypeInfo -> OidCache
fromHashMap byName = OidCache byName

-- * Accessors

{-# INLINE lookupScalar #-}
lookupScalar :: Maybe Text -> Text -> OidCache -> Maybe Word32
lookupScalar schema name (OidCache byName) =
  HashMap.lookup (Kernel.QualifiedTypeName.QualifiedTypeName schema name) byName <&> \info -> Kernel.TypeInfo.toBaseOid info

{-# INLINE lookupArray #-}
lookupArray :: Maybe Text -> Text -> OidCache -> Maybe Word32
lookupArray schema name (OidCache byName) =
  HashMap.lookup (Kernel.QualifiedTypeName.QualifiedTypeName schema name) byName <&> \info -> Kernel.TypeInfo.toArrayOid info

{-# INLINE toHashMap #-}
toHashMap :: OidCache -> HashMap Kernel.QualifiedTypeName Kernel.TypeInfo.TypeInfo
toHashMap (OidCache byName) = byName
