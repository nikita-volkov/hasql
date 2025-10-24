module Codecs.Decoders
  ( -- * Nullability
    NullableOrNot.NullableOrNot (..),
    NullableOrNot.nonNullable,
    NullableOrNot.nullable,

    -- * Value
    Value.Value (..),
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
    Value.jsonb,
    Value.jsonbBytes,
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
    array,
    listArray,
    vectorArray,
    composite,
    record,
    Value.hstore,
    Value.enum,
    Value.custom,
    Value.refine,

    -- * Array
    Array.Array,
    Array.dimension,
    Array.element,

    -- * Composite
    Composite.Composite (..),
    Composite.field,
  )
where

import Codecs.Decoders.Array qualified as Array
import Codecs.Decoders.Composite qualified as Composite
import Codecs.Decoders.NullableOrNot qualified as NullableOrNot
import Codecs.Decoders.Value qualified as Value
import Codecs.TypeInfo qualified as TypeInfo
import Data.Vector.Generic qualified as GenericVector
import Platform.Prelude

-- * Value

-- |
-- Lift an 'Array.Array' decoder to a 'Value.Value' decoder.
{-# INLINEABLE array #-}
array :: Array.Array a -> Value.Value a
array decoder = Value.Value (Array.toSchema decoder) (Array.toTypeName decoder) (Array.toBaseOid decoder) (Array.toArrayOid decoder) (Array.toDimensionality decoder) (Array.toValueDecoder decoder)

-- |
-- Lift a value decoder of element into a unidimensional array decoder producing a list.
--
-- This function is merely a shortcut to the following expression:
--
-- @
-- ('array' . 'dimension' Control.Monad.'replicateM' . 'element')
-- @
--
-- Please notice that in case of multidimensional arrays nesting 'listArray' decoder
-- won't work. You have to explicitly construct the array decoder using 'array'.
{-# INLINE listArray #-}
listArray :: NullableOrNot.NullableOrNot Value.Value element -> Value.Value [element]
listArray = array . Array.dimension replicateM . Array.element

-- |
-- Lift a value decoder of element into a unidimensional array decoder producing a generic vector.
--
-- This function is merely a shortcut to the following expression:
--
-- @
-- ('array' . 'dimension' Data.Vector.Generic.'GenericVector.replicateM' . 'element')
-- @
--
-- Please notice that in case of multidimensional arrays nesting 'vectorArray' decoder
-- won't work. You have to explicitly construct the array decoder using 'array'.
{-# INLINE vectorArray #-}
vectorArray :: (GenericVector.Vector vector element) => NullableOrNot.NullableOrNot Value.Value element -> Value.Value (vector element)
vectorArray = array . Array.dimension GenericVector.replicateM . Array.element

-- |
-- Lift a 'Composite.Composite' decoder to a 'Value.Value' decoder for named composite types.
--
-- This function is for named composite types where the type name is known.
-- For anonymous composite types (like those created with ROW constructor),
-- use 'record' instead.
{-# INLINEABLE composite #-}
composite :: Maybe Text -> Text -> Composite.Composite a -> Value.Value a
composite schema typeName composite =
  Value.Value
    schema
    typeName
    Nothing
    Nothing
    0
    (Composite.toValueDecoder composite)

-- |
-- Lift a 'Composite.Composite' decoder to a 'Value.Value' decoder for unnamed composite types.
--
-- This is useful for decoding anonymous composites (like those created with ROW constructor)
-- where no type name is required. Postgres will handle the type automatically.
{-# INLINEABLE record #-}
record :: Composite.Composite a -> Value.Value a
record composite =
  Value.Value
    Nothing
    "record"
    (Just (TypeInfo.toBaseOid typeInfo))
    (Just (TypeInfo.toArrayOid typeInfo))
    0
    (Composite.toValueDecoder composite)
  where
    typeInfo = TypeInfo.record
