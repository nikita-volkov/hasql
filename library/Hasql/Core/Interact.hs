module Hasql.Core.Interact where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.Query as A


newtype Interact result =
  Interact (F A.Query result)
  deriving (Functor, Applicative, Monad)

query :: A.Query result -> Interact result
query = Interact . liftF
