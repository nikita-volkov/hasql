module Platform.LookingUp where

import Platform.Prelude

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
  LookingUp lKeys lCont <*> LookingUp rKeys rCont =
    LookingUp
      (lKeys <> rKeys)
      (\lookup -> lCont lookup <*> rCont lookup)
