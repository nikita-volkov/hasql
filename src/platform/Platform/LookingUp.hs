module Platform.LookingUp where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Platform.Prelude

data LookingUp k v f a
  = LookingUp
      -- | Keys to populate in the provided map.
      (HashSet k)
      -- | Continuation that looks up values by keys in the provided map.
      (HashMap k v -> f a)

deriving instance (Functor f) => Functor (LookingUp k v f)

instance (Applicative f, Hashable k) => Applicative (LookingUp k v f) where
  pure a =
    LookingUp HashSet.empty (\_ -> pure a)
  LookingUp lKeys lCont <*> LookingUp rKeys rCont =
    LookingUp
      (lKeys <> rKeys)
      (\map -> lCont map <*> rCont map)
