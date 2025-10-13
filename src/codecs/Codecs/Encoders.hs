-- |
-- A DSL for declaration of statement parameter encoders.
--
-- For compactness of names all the types defined here imply being an encoder.
-- E.g., the `Array` type is an __encoder__ of arrays, not the data-structure itself.
module Codecs.Encoders
  ( -- * Parameters product
    Params.Params,
    noParams,
    param,

    -- * Nullability
    NullableOrNot,
    nonNullable,
    nullable,

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
    array,
    foldableArray,
    composite,

    -- * Array
    Array.Array,
    element,
    dimension,

    -- * Composite
    Composite,
    field,
  )
where

import Codecs.Encoders.Array qualified as Array
import Codecs.Encoders.Params qualified as Params
import Codecs.Encoders.Value qualified as Value
import Codecs.TypeInfo qualified as TypeInfo
import Platform.Prelude hiding (bool)
import PostgreSQL.Binary.Encoding qualified as Binary
import TextBuilder qualified

-- * Parameters Product Encoder

-- |
-- No parameters. Same as `mempty` and `conquered`.
noParams :: Params.Params ()
noParams = mempty

-- |
-- Lift a single parameter encoder, with its nullability specified,
-- associating it with a single placeholder.
param :: NullableOrNot Value.Value a -> Params.Params a
param = \case
  NonNullable valueEnc -> Params.value valueEnc
  Nullable valueEnc -> Params.nullableValue valueEnc

-- * Nullability

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

-- * Value

-- |
-- Lift an array encoder into a value encoder.
array :: Array.Array a -> Value.Value a
array (Array.Array valueOid arrayOid arrayEncoder renderer) =
  let encoder input = Binary.array valueOid (arrayEncoder input)
   in Value.Value "array" False (Just arrayOid) (Just arrayOid) encoder renderer

-- |
-- Lift a composite encoder into a value encoder.
composite :: Composite a -> Value.Value a
composite (Composite encode print) =
  Value.unsafeTypeInfo TypeInfo.unknown encodeValue printValue
  where
    encodeValue val =
      Binary.composite $ encode val
    printValue val =
      "ROW (" <> TextBuilder.intercalate ", " (print val) <> ")"

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
foldableArray :: (Foldable foldable) => NullableOrNot Value.Value element -> Value.Value (foldable element)
foldableArray = array . dimension foldl' . element

-- * Array

-- |
-- Lifts a 'Value.Value' encoder into an 'Array.Array' encoder.
element :: NullableOrNot Value.Value a -> Array.Array a
element = \case
  NonNullable (Value.Value _ _ (Just elementOid) (Just arrayOid) encoder renderer) ->
    (Array.value elementOid arrayOid encoder renderer)
  NonNullable (Value.Value _ _ elementOid arrayOid encoder renderer) ->
    (Array.value (fromMaybe (TypeInfo.toBaseOid TypeInfo.unknown) elementOid) (fromMaybe (TypeInfo.toBaseOid TypeInfo.unknown) arrayOid) encoder renderer)
  Nullable (Value.Value _ _ (Just elementOid) (Just arrayOid) encoder renderer) ->
    (Array.nullableValue elementOid arrayOid encoder renderer)
  Nullable (Value.Value _ _ elementOid arrayOid encoder renderer) ->
    (Array.nullableValue (fromMaybe (TypeInfo.toBaseOid TypeInfo.unknown) elementOid) (fromMaybe (TypeInfo.toBaseOid TypeInfo.unknown) arrayOid) encoder renderer)

-- |
-- Encoder of an array dimension,
-- which thus provides support for multidimensional arrays.
--
-- Accepts:
--
-- * An implementation of the left-fold operation,
-- such as @Data.Foldable.'foldl''@,
-- which determines the input value.
--
-- * A component encoder, which can be either another 'dimension' or 'element'.
{-# INLINEABLE dimension #-}
dimension :: (forall a. (a -> b -> a) -> a -> c -> a) -> Array.Array b -> Array.Array c
dimension foldl imp = Array.dimension foldl imp

-- * Composite

-- |
-- Composite or row-types encoder.
data Composite a
  = Composite
      (a -> Binary.Composite)
      (a -> [TextBuilder.TextBuilder])

instance Contravariant Composite where
  contramap f (Composite encode print) =
    Composite (encode . f) (print . f)

instance Divisible Composite where
  divide f (Composite encodeL printL) (Composite encodeR printR) =
    Composite
      (\val -> case f val of (lVal, rVal) -> encodeL lVal <> encodeR rVal)
      (\val -> case f val of (lVal, rVal) -> printL lVal <> printR rVal)
  conquer = mempty

instance Semigroup (Composite a) where
  Composite encodeL printL <> Composite encodeR printR =
    Composite
      (\val -> encodeL val <> encodeR val)
      (\val -> printL val <> printR val)

instance Monoid (Composite a) where
  mempty = Composite mempty mempty

-- | Single field of a row-type.
field :: NullableOrNot Value.Value a -> Composite a
field = \case
  NonNullable (Value.Value _ _ (Just elementOid) _ encode print) ->
    Composite
      (\val -> Binary.field elementOid (encode val))
      (\val -> [print val])
  NonNullable (Value.Value _ _ Nothing _ encode print) ->
    Composite
      (\val -> Binary.field (TypeInfo.toBaseOid TypeInfo.unknown) (encode val))
      (\val -> [print val])
  Nullable (Value.Value _ _ (Just elementOid) _ encode print) ->
    Composite
      ( \val -> case val of
          Nothing -> Binary.nullField elementOid
          Just val -> Binary.field elementOid (encode val)
      )
      ( \val ->
          case val of
            Nothing -> ["NULL"]
            Just val -> [print val]
      )
  Nullable (Value.Value _ _ Nothing _ encode print) ->
    Composite
      ( \val -> case val of
          Nothing -> Binary.nullField (TypeInfo.toBaseOid TypeInfo.unknown)
          Just val -> Binary.field (TypeInfo.toBaseOid TypeInfo.unknown) (encode val)
      )
      ( \val ->
          case val of
            Nothing -> ["NULL"]
            Just val -> [print val]
      )
