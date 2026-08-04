module Hasql.Codecs.Vocab.ParamMeta
  ( ParamMeta (..),
  )
where

import Hasql.Codecs.Vocab.TypeRef (TypeRef)
import Hasql.Platform.Prelude

-- | Per-parameter metadata: type reference, array dimensionality, text-format flag.
data ParamMeta = ParamMeta TypeRef Word Bool
  deriving stock (Eq, Ord, Show, Generic)

instance Hashable ParamMeta
