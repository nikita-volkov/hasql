-- |
-- A DSL for declaration of statement parameter encoders.
--
-- For compactness of names all the types defined here imply being an encoder.
-- E.g., the `Array` type is an __encoder__ of arrays, not the data-structure itself.
module Hasql.Codecs.Encoders
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
    Value.varchar,
    Value.bpchar,
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
    Value.citext,
    Value.name,
    Value.oid,
    foldableArray,
    array,
    Value.hstore,
    Value.enum,
    composite,
    Value.unknown,
    Value.custom,

    -- * Array
    Array.Array,
    Array.element,
    Array.dimension,

    -- * Composite
    Composite.Composite,
    Composite.field,
  )
where

import Hasql.Codecs.Encoders.Array qualified as Array
import Hasql.Codecs.Encoders.Composite qualified as Composite
import Hasql.Codecs.Encoders.NullableOrNot qualified as NullableOrNot
import Hasql.Codecs.Encoders.Params qualified as Params
import Hasql.Codecs.Encoders.Value qualified as Value
import Hasql.CodecsCore.QualifiedTypeName qualified as CodecsCore.QualifiedTypeName
import Hasql.CodecsCore.RequestingOid qualified as RequestingOid
import Hasql.CodecsCore.TypeInfo qualified as CodecsCore.TypeInfo
import Hasql.Platform.Prelude hiding (bool)
import PostgreSQL.Binary.Encoding qualified as Binary
import TextBuilder qualified

-- * Recursive definitions

-- |
-- Lift a value encoder of element into a unidimensional array encoder of a foldable value.
--
-- This function is merely a shortcut to the following expression:
--
-- @
-- ('array' . 'Array.dimension' 'foldl'' . 'Array.element')
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
array (Array.Array baseTypeSchema baseTypeName _isText dimensionality scalarOidIfKnown arrayOidIfKnown arrayEncoder renderer) =
  let toEncoder baseOid encode = \input -> Binary.array baseOid (encode input)
      encoder = case scalarOidIfKnown of
        Just oid -> RequestingOid.hoist (toEncoder oid) arrayEncoder
        Nothing ->
          RequestingOid.hoistLookingUp
            (CodecsCore.QualifiedTypeName.QualifiedTypeName baseTypeSchema baseTypeName)
            (\typeInfo -> toEncoder (CodecsCore.TypeInfo.toBaseOid typeInfo))
            arrayEncoder
   in Value.Value baseTypeSchema baseTypeName scalarOidIfKnown arrayOidIfKnown dimensionality False encoder renderer

-- |
-- Lift a composite encoder into a value encoder for named composite types.
--
-- This function is for named composite types where the type name is known.
-- If you need to encode an anonymous composite type (like those created with the ROW constructor),
-- PostgreSQL itself does not support that.
composite ::
  -- | Schema name where the composite type is defined.
  Maybe Text ->
  -- | Composite type name.
  Text ->
  Composite.Composite a ->
  Value.Value a
composite schema name (Composite.Composite request print) =
  Value.Value schema name Nothing Nothing 0 False encoder printValue
  where
    encoder = RequestingOid.hoist (Binary.composite .) request
    printValue val =
      "ROW (" <> TextBuilder.intercalate ", " (print val) <> ")"
