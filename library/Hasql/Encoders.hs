{-|
A DSL for declaration of statement parameter encoders.

For compactness of names all the types defined here imply being an encoder.
E.g., the `Array` type is an __encoder__ of arrays, not the data-structure itself.
-}
module Hasql.Encoders
(
  -- * Parameters product
  Params,
  noParams,
  param,
  -- * Nullability
  NullableOrNot,
  nonNullable,
  nullable,
  -- * Value
  Value,
  bool,
  int2,
  int4,
  int8,
  float4,
  float8,
  numeric,
  char,
  text,
  bytea,
  date,
  timestamp,
  timestamptz,
  time,
  timetz,
  interval,
  uuid,
  inet,
  json,
  jsonBytes,
  jsonb,
  jsonbBytes,
  enum,
  unknown,
  unknownLazy,
  unknown',
  array,
  foldableArray,
  -- * Array
  Array,
  element,
  dimension,
)
where

import Hasql.Private.Encoders
