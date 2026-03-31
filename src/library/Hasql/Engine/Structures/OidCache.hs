module Hasql.Engine.Structures.OidCache
  ( OidCache,

    -- * Accessors
    toHashMap,
    lookupScalar,
    lookupArray,
    getDomainMap,
    domainMapLoaded,

    -- * Constructors
    fromHashMap,
    empty,
    selectUnknownNames,
    insertScalar,
    setDomainMap,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Hasql.Platform.Prelude hiding (empty, insert, lookup, reset)

-- | Pure registry state containing the hash map and counter
data OidCache
  = OidCache
      -- | By name of the type.
      --
      -- > scalar name -> (scalar OID, array OID)
      (HashMap (Maybe Text, Text) (Word32, Word32))
      -- | Domain OID to base type OID mapping.
      --
      -- > domain OID -> base type OID
      (HashMap Word32 Word32)
      -- | Whether the domain map has been loaded.
      Bool
  deriving stock (Show, Eq)

instance Semigroup OidCache where
  OidCache byNameL domainL loadedL <> OidCache byNameR domainR loadedR =
    OidCache (HashMap.union byNameR byNameL) (HashMap.union domainR domainL) (loadedL || loadedR)

instance Monoid OidCache where
  mempty = OidCache mempty mempty False

{-# INLINEABLE empty #-}
empty :: OidCache
empty =
  OidCache HashMap.empty HashMap.empty False

-- | Having a set of required type names, select those that are not present in the cache.
selectUnknownNames :: HashSet (Maybe Text, Text) -> OidCache -> HashSet (Maybe Text, Text)
selectUnknownNames keys (OidCache byName _ _) =
  HashSet.filter (\key -> not (HashMap.member key byName)) keys

insertScalar :: Maybe Text -> Text -> Word32 -> Word32 -> OidCache -> OidCache
insertScalar schema name scalar array (OidCache byName domain loaded) =
  OidCache (HashMap.insert (schema, name) (scalar, array) byName) domain loaded

fromHashMap :: HashMap (Maybe Text, Text) (Word32, Word32) -> OidCache
fromHashMap byName = OidCache byName HashMap.empty False

-- * Accessors

lookupScalar :: Maybe Text -> Text -> OidCache -> Maybe Word32
lookupScalar schema name (OidCache byName _ _) =
  HashMap.lookup (schema, name) byName <&> \(scalar, _) -> scalar

lookupArray :: Maybe Text -> Text -> OidCache -> Maybe Word32
lookupArray schema name (OidCache byName _ _) =
  HashMap.lookup (schema, name) byName <&> \(_, array) -> array

toHashMap :: OidCache -> HashMap (Maybe Text, Text) (Word32, Word32)
toHashMap (OidCache byName _ _) = byName

-- | Get the domain OID to base type OID mapping.
getDomainMap :: OidCache -> HashMap Word32 Word32
getDomainMap (OidCache _ domainMap _) = domainMap

-- | Whether the domain map has been loaded from the database.
domainMapLoaded :: OidCache -> Bool
domainMapLoaded (OidCache _ _ loaded) = loaded

-- | Set the domain OID map and mark it as loaded.
setDomainMap :: HashMap Word32 Word32 -> OidCache -> OidCache
setDomainMap domainMap (OidCache byName _ _) = OidCache byName domainMap True
