module Hasql.ToBeResolved
  ( ToBeResolved (..),
    lookup,
    augmentedBy,
  )
where

import Control.Applicative
import Prelude hiding (lookup)

-- |
-- A computation that first declares the keys it needs resolved and then,
-- once a resolver @k -> v@ is provided, produces its result.
--
-- The defining trait is the upfront collection of keys prior to resolution,
-- hence the name.
data ToBeResolved k v a
  = ToBeResolved
      -- | Keys requested to be available for lookup.
      [k]
      -- | Continuation that looks up values by keys.
      ((k -> v) -> a)

type role ToBeResolved _ _ representational

deriving stock instance Functor (ToBeResolved k v)

instance Applicative (ToBeResolved k v) where
  {-# INLINE pure #-}
  pure a =
    ToBeResolved [] (\_ -> a)
  {-# INLINE (<*>) #-}
  ToBeResolved lKeys lUse <*> ToBeResolved rKeys rUse =
    ToBeResolved
      (lKeys <> rKeys)
      (\lookup -> lUse lookup (rUse lookup))

{-# INLINE lookup #-}
lookup :: k -> ToBeResolved k v v
lookup key =
  ToBeResolved [key] (\lookupFn -> lookupFn key)

{-# INLINE augmentedBy #-}
augmentedBy :: k -> (v -> a -> b) -> ToBeResolved k v a -> ToBeResolved k v b
augmentedBy k tx (ToBeResolved keys use) =
  ToBeResolved (k : keys) (\lookupFn -> tx (lookupFn k) (use lookupFn))
