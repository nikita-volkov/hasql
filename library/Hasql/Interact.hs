module Hasql.Interact where

import Hasql.Prelude
import Hasql.Model
import qualified Hasql.Query as A


newtype Interact result =
  Interact (F A.Query result)
  deriving (Functor, Applicative, Monad)

query :: A.Query result -> Interact result
query = Interact . liftF
