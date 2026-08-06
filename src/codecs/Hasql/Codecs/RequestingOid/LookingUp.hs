module Hasql.Codecs.RequestingOid.LookingUp where

import Control.Applicative
import Prelude

data LookingUp k v a
  = LookingUp
      -- | Keys requested to be available for lookup.
      [k]
      -- | Continuation that looks up values by keys.
      ((k -> v) -> a)

type role LookingUp _ _ representational

deriving stock instance Functor (LookingUp k v)

instance Applicative (LookingUp k v) where
  {-# INLINE pure #-}
  pure a =
    LookingUp [] (\_ -> a)
  {-# INLINE (<*>) #-}
  LookingUp lKeys lUse <*> LookingUp rKeys rUse =
    LookingUp
      (lKeys <> rKeys)
      (\lookup -> lUse lookup (rUse lookup))

{-# INLINE lookup #-}
lookup :: k -> LookingUp k v v
lookup key =
  LookingUp [key] (\lookupFn -> lookupFn key)

{-# INLINE lift #-}
lift :: a -> LookingUp k v a
lift fa =
  LookingUp [] (const fa)

{-# INLINE lookingUp #-}
lookingUp :: k -> (v -> a) -> LookingUp k v a
lookingUp key cont =
  LookingUp [key] (\lookupFn -> cont (lookupFn key))

{-# INLINE hoistLookingUp #-}
hoistLookingUp :: k -> (v -> a -> b) -> LookingUp k v a -> LookingUp k v b
hoistLookingUp k tx (LookingUp keys use) =
  LookingUp (k : keys) (\lookupFn -> tx (lookupFn k) (use lookupFn))
