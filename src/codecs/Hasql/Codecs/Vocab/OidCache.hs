module Hasql.Codecs.Vocab.OidCache
  ( OidCache,

    -- * Accessors
    toHashMap,
    lookupScalar,
    lookupArray,
    lookupTypeNameScalar,
    lookupTypeNameArray,
    lookupTypeInfo,

    -- * Constructors
    fromHashMap,
    empty,
    selectUnknownNames,
    insertScalar,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Hasql.Codecs.Vocab.QualifiedTypeName (QualifiedTypeName)
import Hasql.Codecs.Vocab.QualifiedTypeName qualified as QualifiedTypeName
import Hasql.Codecs.Vocab.TypeInfo qualified as TypeInfo
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

insertScalar :: Maybe Text -> Text -> Word32 -> Word32 -> OidCache -> OidCache
insertScalar schema name scalar array (OidCache byName) =
  OidCache (HashMap.insert (QualifiedTypeName.QualifiedTypeName schema name) (TypeInfo.TypeInfo scalar array) byName)

{-# INLINE fromHashMap #-}
fromHashMap :: HashMap QualifiedTypeName TypeInfo.TypeInfo -> OidCache
fromHashMap byName = OidCache byName

-- * Accessors

{-# INLINE lookupScalar #-}
lookupScalar :: Maybe Text -> Text -> OidCache -> Maybe Word32
lookupScalar schema name (OidCache byName) =
  HashMap.lookup (QualifiedTypeName.QualifiedTypeName schema name) byName <&> \info -> TypeInfo.toBaseOid info

{-# INLINE lookupArray #-}
lookupArray :: Maybe Text -> Text -> OidCache -> Maybe Word32
lookupArray schema name (OidCache byName) =
  HashMap.lookup (QualifiedTypeName.QualifiedTypeName schema name) byName <&> \info -> TypeInfo.toArrayOid info

{-# INLINE lookupTypeNameScalar #-}
lookupTypeNameScalar :: QualifiedTypeName -> OidCache -> Maybe Word32
lookupTypeNameScalar name (OidCache byName) =
  HashMap.lookup name byName <&> TypeInfo.toBaseOid

{-# INLINE lookupTypeNameArray #-}
lookupTypeNameArray :: QualifiedTypeName -> OidCache -> Maybe Word32
lookupTypeNameArray name (OidCache byName) =
  HashMap.lookup name byName <&> TypeInfo.toArrayOid

{-# INLINE lookupTypeInfo #-}
lookupTypeInfo :: QualifiedTypeName -> OidCache -> Maybe TypeInfo.TypeInfo
lookupTypeInfo name (OidCache byName) =
  HashMap.lookup name byName

{-# INLINE toHashMap #-}
toHashMap :: OidCache -> HashMap QualifiedTypeName TypeInfo.TypeInfo
toHashMap (OidCache byName) = byName
