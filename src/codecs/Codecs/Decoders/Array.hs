module Codecs.Decoders.Array
  ( Array,
    toValueDecoder,
    toTypeName,
    toOid,
    toSchema,
    toElementTypeName,
    dimension,
    element,
  )
where

import Codecs.Decoders.NullableOrNot qualified as NullableOrNot
import Codecs.Decoders.Value qualified as Value
import Codecs.RequestingOid qualified as RequestingOid
import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as Binary
import TextBuilder qualified

-- |
-- Binary generic array decoder.
--
-- Here's how you can use it to produce a specific array value decoder:
--
-- @
-- x :: 'Value.Value' [[Text]]
-- x = 'array' ('dimension' 'replicateM' ('dimension' 'replicateM' ('element' ('nonNullable' 'text'))))
-- @
data Array a
  = Array
      -- | Schema name.
      (Maybe Text)
      -- | Type name for the array element.
      Text
      -- | Statically known OID for the array type.
      (Maybe Word32)
      -- | Number of dimensions.
      Int
      -- | Decoding function
      (RequestingOid.RequestingOid Binary.Array a)
  deriving (Functor)

{-# INLINE toValueDecoder #-}
toValueDecoder :: Array a -> RequestingOid.RequestingOid Binary.Value a
toValueDecoder (Array _ _ _ _ decoder) =
  RequestingOid.hoist Binary.array decoder

-- | Get the type name for the array based on element type name
{-# INLINE toTypeName #-}
toTypeName :: Array a -> Text
toTypeName (Array _ elementTypeName _ ndims _) =
  let chunks =
        TextBuilder.text elementTypeName
          : replicate ndims (TextBuilder.text "[]")
   in TextBuilder.toText (mconcat chunks)

-- | Get the array OID if statically known
{-# INLINE toOid #-}
toOid :: Array a -> Maybe Word32
toOid (Array _ _ oid _ _) = oid

-- | Get the schema name for the element type
{-# INLINE toSchema #-}
toSchema :: Array a -> Maybe Text
toSchema (Array schema _ _ _ _) = schema

-- | Get the element type name
{-# INLINE toElementTypeName #-}
toElementTypeName :: Array a -> Text
toElementTypeName (Array _ typeName _ _ _) = typeName

-- * Public API

-- |
-- Binary function for parsing a dimension of an array.
-- Provides support for multi-dimensional arrays.
--
-- Accepts:
--
-- * An implementation of the @replicateM@ function
-- (@Control.Monad.'Control.Monad.replicateM'@, @Data.Vector.'Data.Vector.replicateM'@),
-- which determines the output value.
--
-- * Binary decoder of its components, which can be either another 'dimension' or 'element'.
{-# INLINEABLE dimension #-}
dimension :: (forall m. (Monad m) => Int -> m a -> m b) -> Array a -> Array b
dimension replicateM (Array schema typeName typeOid ndims decoder) =
  Array
    schema
    typeName
    typeOid
    (succ ndims)
    (RequestingOid.hoist (Binary.dimensionArray replicateM) decoder)

-- |
-- Lift a 'Value.Value' decoder into an 'Array' decoder for parsing of leaf values.
{-# INLINEABLE element #-}
element :: NullableOrNot.NullableOrNot Value.Value a -> Array a
element = \case
  NullableOrNot.NonNullable imp ->
    Array
      (Value.toSchema imp)
      (Value.toTypeName imp)
      (Value.toArrayOid imp)
      1
      (RequestingOid.hoist Binary.valueArray (Value.toDecoder imp))
  NullableOrNot.Nullable imp ->
    Array
      (Value.toSchema imp)
      (Value.toTypeName imp)
      (Value.toArrayOid imp)
      1
      (RequestingOid.hoist Binary.nullableValueArray (Value.toDecoder imp))
