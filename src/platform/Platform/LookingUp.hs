module Platform.LookingUp where

import Control.Applicative
import Witherable
import Prelude

data LookingUp k v f a
  = LookingUp
      -- | Keys requested to be available for lookup.
      [k]
      -- | Continuation that looks up values by keys.
      ((k -> v) -> f a)

deriving instance (Functor f) => Functor (LookingUp k v f)

instance (Applicative f) => Applicative (LookingUp k v f) where
  pure a =
    LookingUp [] (\_ -> pure a)
  LookingUp lKeys lUse <*> LookingUp rKeys rUse =
    LookingUp
      (lKeys <> rKeys)
      (\lookup -> lUse lookup <*> rUse lookup)

instance (Filterable f) => Filterable (LookingUp k v f) where
  {-# INLINE mapMaybe #-}
  mapMaybe fn (LookingUp keys use) =
    LookingUp keys (mapMaybe fn . use)

lookup :: (Applicative f) => k -> LookingUp k v f v
lookup key =
  LookingUp [key] (\lookupFn -> pure (lookupFn key))

lift :: f a -> LookingUp k v f a
lift fa =
  LookingUp [] (const fa)

hoist :: (f a -> g b) -> LookingUp k v f a -> LookingUp k v g b
hoist tx (LookingUp keys use) =
  LookingUp keys (tx . use)

lookingUp :: k -> (v -> f a) -> LookingUp k v f a
lookingUp key cont =
  LookingUp [key] (\lookupFn -> cont (lookupFn key))

hoistLookingUp :: k -> (v -> f a -> g b) -> LookingUp k v f a -> LookingUp k v g b
hoistLookingUp k tx (LookingUp keys use) =
  LookingUp (k : keys) (\lookupFn -> tx (lookupFn k) (use lookupFn))
