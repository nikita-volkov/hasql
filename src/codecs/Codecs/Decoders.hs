module Codecs.Decoders
  ( -- * Nullability
    NullableOrNot (..),
    nonNullable,
    nullable,

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
    Value.hstore,
    enum,
    Value.custom,
    Value.refine,

    -- * Array
    Array (..),
    dimension,
    element,

    -- * Composite
    Composite (..),
    field,
  )
where

import Codecs.Decoders.Array qualified as Array
import Codecs.Decoders.Composite qualified as Composite
import Codecs.Decoders.Value qualified as Value
import Data.Vector.Generic qualified as GenericVector
import Platform.Prelude hiding (bool, maybe)
import PostgreSQL.Binary.Decoding qualified as A

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

-- * Value

-- |
-- Given a partial mapping from text to value,
-- produces a decoder of that value.
enum :: (Text -> Maybe a) -> Value.Value a
enum mapping = Value.decoder (A.enum mapping)

-- |
-- Lift an 'Array' decoder to a 'Value.Value' decoder.
{-# INLINEABLE array #-}
array :: Array a -> Value.Value a
array (Array decoder) = Value.Value (Array.toTypeName decoder) (Array.toOid decoder) Nothing (Array.toDecoder decoder)

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
listArray :: NullableOrNot Value.Value element -> Value.Value [element]
listArray = array . dimension replicateM . element

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
vectorArray :: (GenericVector.Vector vector element) => NullableOrNot Value.Value element -> Value.Value (vector element)
vectorArray = array . dimension GenericVector.replicateM . element

-- |
-- Lift a 'Composite' decoder to a 'Value.Value' decoder.
{-# INLINEABLE composite #-}
composite :: Composite a -> Value.Value a
composite (Composite imp) = Value.Value "unknown" Nothing Nothing (Composite.run imp)

-- * Array decoders

-- |
-- A generic array decoder.
--
-- Here's how you can use it to produce a specific array value decoder:
--
-- @
-- x :: 'Value.Value' [[Text]]
-- x = 'array' ('dimension' 'replicateM' ('dimension' 'replicateM' ('element' ('nonNullable' 'text'))))
-- @
newtype Array a = Array (Array.ArrayDecoder a)
  deriving (Functor)

-- |
-- A function for parsing a dimension of an array.
-- Provides support for multi-dimensional arrays.
--
-- Accepts:
--
-- * An implementation of the @replicateM@ function
-- (@Control.Monad.'Control.Monad.replicateM'@, @Data.Vector.'Data.Vector.replicateM'@),
-- which determines the output value.
--
-- * A decoder of its components, which can be either another 'dimension' or 'element'.
{-# INLINEABLE dimension #-}
dimension :: (forall m. (Monad m) => Int -> m a -> m b) -> Array a -> Array b
dimension replicateM (Array imp) = Array (Array.dimension replicateM imp)

-- |
-- Lift a 'Value.Value' decoder into an 'Array' decoder for parsing of leaf values.
{-# INLINEABLE element #-}
element :: NullableOrNot Value.Value a -> Array a
element = \case
  NonNullable imp ->
    Array
      ( Array.nonNullableElement
          (Value.toTypeName imp)
          (Value.toArrayOid imp)
          (Value.toHandler imp)
      )
  Nullable imp ->
    Array
      ( Array.nullableElement
          (Value.toTypeName imp)
          (Value.toArrayOid imp)
          (Value.toHandler imp)
      )

-- * Composite decoders

-- |
-- Composable decoder of composite values (rows, records).
newtype Composite a = Composite (Composite.CompositeDecoder a)
  deriving newtype (Functor, Applicative, Monad, MonadFail)

-- |
-- Lift a 'Value.Value' decoder into a 'Composite' decoder for parsing of component values.
field :: NullableOrNot Value.Value a -> Composite a
field = \case
  NonNullable imp -> Composite (Composite.nonNullValue (Value.toHandler imp))
  Nullable imp -> Composite (Composite.value (Value.toHandler imp))
