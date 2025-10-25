module Core.Structures.OidCache
  ( OidCache,

    -- * Accessors
    toHashMap,

    -- * Constructors
    fromHashMap,
    empty,
    selectUnknownNames,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Platform.Prelude hiding (empty, insert, lookup, reset)

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

fromHashMap :: HashMap (Maybe Text, Text) (Word32, Word32) -> OidCache
fromHashMap byName = OidCache byName

-- * Accessors

toHashMap :: OidCache -> HashMap (Maybe Text, Text) (Word32, Word32)
toHashMap (OidCache byName) = byName
