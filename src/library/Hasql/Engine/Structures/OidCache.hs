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
import Hasql.Platform.Prelude hiding (empty, insert, lookup, reset)

-- | Pure registry state containing the hash map and counter
newtype OidCache
  = OidCache
      -- | By name of the type.
      --
      -- > scalar name -> (scalar OID, array OID)
      (HashMap (Maybe Text, Text) (Word32, Word32))
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
selectUnknownNames :: HashSet (Maybe Text, Text) -> OidCache -> HashSet (Maybe Text, Text)
selectUnknownNames keys (OidCache byName) =
  HashSet.filter (\key -> not (HashMap.member key byName)) keys

insertScalar :: Maybe Text -> Text -> Word32 -> Word32 -> OidCache -> OidCache
insertScalar schema name scalar array (OidCache byName) =
  OidCache (HashMap.insert (schema, name) (scalar, array) byName)

fromHashMap :: HashMap (Maybe Text, Text) (Word32, Word32) -> OidCache
fromHashMap byName = OidCache byName

-- * Accessors

lookupScalar :: Maybe Text -> Text -> OidCache -> Maybe Word32
lookupScalar schema name (OidCache byName) =
  HashMap.lookup (schema, name) byName <&> \(scalar, _) -> scalar

lookupArray :: Maybe Text -> Text -> OidCache -> Maybe Word32
lookupArray schema name (OidCache byName) =
  HashMap.lookup (schema, name) byName <&> \(_, array) -> array

toHashMap :: OidCache -> HashMap (Maybe Text, Text) (Word32, Word32)
toHashMap (OidCache byName) = byName
