-- |
-- A DSL for declaration of statement parameter encoders.
--
-- For compactness of names all the types defined here imply being an encoder.
-- E.g., the `Array` type is an __encoder__ of arrays, not the data-structure itself.
module Codecs.Encoders
  ( -- * Parameters product
    Params.Params,
    Params.noParams,
    Params.param,

    -- * Nullability
    NullableOrNot.NullableOrNot,
    NullableOrNot.nonNullable,
    NullableOrNot.nullable,

    -- * Value
    Value.Value,
    Value.bool,
    Value.int2,
    Value.int4,
    Value.int8,
    Value.float4,
    Value.float8,
    Value.numeric,
    Value.char,
    Value.text,
    Value.bytea,
    Value.date,
    Value.timestamp,
    Value.timestamptz,
    Value.time,
    Value.timetz,
    Value.interval,
    Value.uuid,
    Value.inet,
    Value.macaddr,
    Value.json,
    Value.jsonBytes,
    Value.jsonLazyBytes,
    Value.jsonb,
    Value.jsonbBytes,
    Value.jsonbLazyBytes,
    Value.int4range,
    Value.int8range,
    Value.numrange,
    Value.tsrange,
    Value.tstzrange,
    Value.daterange,
    Value.int4multirange,
    Value.int8multirange,
    Value.nummultirange,
    Value.tsmultirange,
    Value.tstzmultirange,
    Value.datemultirange,
    Value.name,
    Value.oid,
    Value.enum,
    Value.unknownEnum,
    Value.unknown,
    foldableArray,
    array,
    composite,

    -- * Array
    Array.Array,
    Array.element,
    Array.dimension,

    -- * Composite
    Composite.Composite,
    Composite.field,
  )
where

import Codecs.Encoders.Array qualified as Array
import Codecs.Encoders.Composite qualified as Composite
import Codecs.Encoders.NullableOrNot qualified as NullableOrNot
import Codecs.Encoders.Params qualified as Params
import Codecs.Encoders.Value qualified as Value
import Platform.Prelude
import PostgreSQL.Binary.Encoding qualified as Binary
import TextBuilder qualified

-- * Recursive definitions

-- |
-- Lift a value encoder of element into a unidimensional array encoder of a foldable value.
--
-- This function is merely a shortcut to the following expression:
--
-- @
-- ('array' . 'dimension' 'foldl'' . 'element')
-- @
--
-- You can use it like this:
--
-- @
-- vectorOfInts :: Value (Vector Int64)
-- vectorOfInts = 'foldableArray' ('nonNullable' 'int8')
-- @
--
-- Please notice that in case of multidimensional arrays nesting 'foldableArray' encoder
-- won't work. You have to explicitly construct the array encoder using 'array'.
{-# INLINE foldableArray #-}
foldableArray :: (Foldable foldable) => NullableOrNot.NullableOrNot Value.Value element -> Value.Value (foldable element)
foldableArray = array . Array.dimension foldl' . Array.element

-- |
-- Lift an array encoder into a value encoder.
array :: Array.Array a -> Value.Value a
array (Array.Array valueOid arrayOid arrayEncoder renderer) =
  let encoder input = Binary.array valueOid (arrayEncoder input)
   in Value.Value "array" False (Just arrayOid) (Just arrayOid) encoder renderer

-- |
-- Lift a composite encoder into a value encoder.
composite :: Composite.Composite a -> Value.Value a
composite (Composite.Composite encode print) =
  Value.unknownPrimitive encodeValue printValue
  where
    encodeValue val =
      Binary.composite $ encode val
    printValue val =
      "ROW (" <> TextBuilder.intercalate ", " (print val) <> ")"
