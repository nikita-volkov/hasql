module Hasql.ToBeResolved
  ( ToBeResolved (..),
    lookup,
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

-- Written out rather than derived so that 'fmap' carries an @INLINE@. Every
-- row decoder is a chain of 'fmap's and '<*>'s over this type, and without the
-- unfolding the chain cannot collapse at its definition site -- which leaves a
-- closure allocation per column per row.
instance Functor (ToBeResolved k v) where
  {-# INLINE fmap #-}
  fmap fn (ToBeResolved keys use) =
    ToBeResolved keys (fn . use)

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
