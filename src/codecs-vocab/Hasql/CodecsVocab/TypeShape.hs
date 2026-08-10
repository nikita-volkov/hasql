module Hasql.CodecsVocab.TypeShape
  ( TypeShape (..),
  )
where

import Hasql.CodecsVocab.TypeRef (TypeRef)
import Hasql.Platform.Prelude

-- | A value's type shape: type reference, array dimensionality, text-format flag.
data TypeShape = TypeShape TypeRef Word Bool
  deriving stock (Eq, Ord, Show, Generic)

instance Hashable TypeShape
