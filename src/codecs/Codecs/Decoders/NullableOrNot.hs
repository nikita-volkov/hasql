module Codecs.Decoders.NullableOrNot
  ( -- * Nullability
    NullableOrNot (..),
    nonNullable,
    nullable,
  )
where

import Platform.Prelude

-- * Nullability

-- |
-- Extensional specification of nullability over a generic decoder.
data NullableOrNot decoder a where
  NonNullable :: decoder a -> NullableOrNot decoder a
  Nullable :: decoder a -> NullableOrNot decoder (Maybe a)

-- |
-- Specify that a decoder produces a non-nullable value.
nonNullable :: decoder a -> NullableOrNot decoder a
nonNullable = NonNullable

-- |
-- Specify that a decoder produces a nullable value.
nullable :: decoder a -> NullableOrNot decoder (Maybe a)
nullable = Nullable
