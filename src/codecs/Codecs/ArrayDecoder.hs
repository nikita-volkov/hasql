module Codecs.ArrayDecoder
  ( ArrayDecoder,
    toDecoder,
    toTypeName,
    toOid,
    dimension,
    nullableElement,
    nonNullableElement,
  )
where

import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as Binary
import TextBuilder qualified

data ArrayDecoder a
  = ArrayDecoder
      -- | Type name for the array element
      Text
      -- | Statically known OID for the array type.
      (Maybe Word32)
      -- | Number of dimensions.
      Int
      -- | Decoding function
      (Binary.Array a)
  deriving (Functor)

{-# INLINE toDecoder #-}
toDecoder :: ArrayDecoder a -> Binary.Value a
toDecoder (ArrayDecoder _ _ _ decoder) =
  Binary.array decoder

-- | Get the type name for the array based on element type name
{-# INLINE toTypeName #-}
toTypeName :: ArrayDecoder a -> Text
toTypeName (ArrayDecoder elementTypeName _ ndims _) =
  let chunks =
        TextBuilder.text elementTypeName
          : replicate ndims (TextBuilder.text "[]")
   in TextBuilder.toText (mconcat chunks)

-- | Get the array OID if statically known
{-# INLINE toOid #-}
toOid :: ArrayDecoder a -> Maybe Word32
toOid (ArrayDecoder _ oid _ _) = oid

{-# INLINE dimension #-}
dimension :: (forall m. (Monad m) => Int -> m a -> m b) -> ArrayDecoder a -> ArrayDecoder b
dimension replicateM (ArrayDecoder typeName typeOid ndims decoder) =
  ArrayDecoder typeName typeOid (succ ndims) $ Binary.dimensionArray replicateM decoder

{-# INLINE nullableElement #-}
nullableElement :: Text -> Maybe Word32 -> Binary.Value a -> ArrayDecoder (Maybe a)
nullableElement elementTypeName oid decoder =
  ArrayDecoder elementTypeName oid 1 $ Binary.nullableValueArray decoder

{-# INLINE nonNullableElement #-}
nonNullableElement :: Text -> Maybe Word32 -> Binary.Value a -> ArrayDecoder a
nonNullableElement elementTypeName oid decoder =
  ArrayDecoder elementTypeName oid 1 $ Binary.valueArray decoder
