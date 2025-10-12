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

import Codecs.PostgresTypeInfo qualified as PTI
import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as A
import TextBuilder qualified

data ArrayDecoder a
  = ArrayDecoder
      -- | Type name for the array element
      Text
      -- | Statically known OID for the array type.
      (Maybe PTI.OID)
      -- | Number of dimensions.
      Int
      -- | Decoding function
      (A.Array a)
  deriving (Functor)

{-# INLINE toDecoder #-}
toDecoder :: ArrayDecoder a -> A.Value a
toDecoder (ArrayDecoder _ _ _ decoder) =
  A.array decoder

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
toOid :: ArrayDecoder a -> Maybe PTI.OID
toOid (ArrayDecoder _ oid _ _) = oid

{-# INLINE dimension #-}
dimension :: (forall m. (Monad m) => Int -> m a -> m b) -> ArrayDecoder a -> ArrayDecoder b
dimension replicateM (ArrayDecoder typeName typeOID ndims decoder) =
  ArrayDecoder typeName typeOID (succ ndims) $ A.dimensionArray replicateM decoder

{-# INLINE nullableElement #-}
nullableElement :: Text -> Maybe PTI.OID -> A.Value a -> ArrayDecoder (Maybe a)
nullableElement elementTypeName oid decoder =
  ArrayDecoder elementTypeName oid 1 $ A.nullableValueArray decoder

{-# INLINE nonNullableElement #-}
nonNullableElement :: Text -> Maybe PTI.OID -> A.Value a -> ArrayDecoder a
nonNullableElement elementTypeName oid decoder =
  ArrayDecoder elementTypeName oid 1 $ A.valueArray decoder
