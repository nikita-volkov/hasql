module Hasql.Core.Session where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.Batch as A


newtype Session result =
  Session (F A.Batch result)
  deriving (Functor, Applicative, Monad)

batch :: A.Batch result -> Session result
batch = Session . liftF
