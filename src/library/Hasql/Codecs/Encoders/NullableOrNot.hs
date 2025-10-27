module Hasql.Codecs.Encoders.NullableOrNot where

import Hasql.Platform.Prelude

-- |
-- Extensional specification of nullability over a generic encoder.
data NullableOrNot encoder a where
  NonNullable :: encoder a -> NullableOrNot encoder a
  Nullable :: encoder a -> NullableOrNot encoder (Maybe a)

-- |
-- Specify that an encoder produces a non-nullable value.
nonNullable :: encoder a -> NullableOrNot encoder a
nonNullable = NonNullable

-- |
-- Specify that an encoder produces a nullable value.
nullable :: encoder a -> NullableOrNot encoder (Maybe a)
nullable = Nullable
