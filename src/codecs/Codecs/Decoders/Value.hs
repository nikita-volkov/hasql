module Codecs.Decoders.Value where

import Codecs.TypeInfo qualified as TypeInfo
import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as Binary

data ValueDecoder a
  = ValueDecoder
      -- | Type name.
      Text
      -- | Statically known OID for the type.
      (Maybe Word32)
      -- | Statically known OID for the array-type with this type as the element.
      (Maybe Word32)
      -- | Decoding function (always integer timestamps for PostgreSQL 10+).
      (Binary.Value a)
  deriving (Functor)

instance Filterable ValueDecoder where
  {-# INLINE mapMaybe #-}
  mapMaybe fn =
    refine (maybe (Left "Invalid value") Right . fn)

{-# INLINE decoder #-}
decoder :: Binary.Value a -> ValueDecoder a
decoder aDecoder =
  {-# SCC "decoder" #-}
  ValueDecoder "unknown" Nothing Nothing aDecoder

{-# INLINE decoderFn #-}
decoderFn :: (Bool -> ByteString -> Either Text a) -> ValueDecoder a
decoderFn fn =
  ValueDecoder
    "unknown"
    Nothing
    Nothing
    (Binary.fn $ fn True) -- Always use integer timestamps

-- |
-- Refine a value decoder, lifting the possible error to the session level.
{-# INLINE refine #-}
refine :: (a -> Either Text b) -> ValueDecoder a -> ValueDecoder b
refine fn (ValueDecoder typeName typeOid arrayOid decoder) =
  ValueDecoder typeName typeOid arrayOid (Binary.refine fn decoder)

-- |
-- Create a decoder from TypeInfo metadata and a decoding function.
{-# INLINE unsafeTypeInfo #-}
unsafeTypeInfo :: Text -> TypeInfo.TypeInfo -> Binary.Value a -> ValueDecoder a
unsafeTypeInfo typeName pti intDecoder =
  ValueDecoder typeName (Just (TypeInfo.toBaseOid pti)) (Just (TypeInfo.toArrayOid pti)) intDecoder

-- * Relations

toTypeName :: ValueDecoder a -> Text
toTypeName (ValueDecoder typeName _ _ _) = typeName

toBaseOid :: ValueDecoder a -> Maybe Word32
toBaseOid (ValueDecoder _ typeOid _ _) =
  typeOid

toArrayOid :: ValueDecoder a -> Maybe Word32
toArrayOid (ValueDecoder _ _ oid _) = oid

{-# INLINE toHandler #-}
toHandler :: ValueDecoder a -> Binary.Value a
toHandler (ValueDecoder _ _ _ decoder) = decoder

{-# INLINE toByteStringParser #-}
toByteStringParser :: ValueDecoder a -> (ByteString -> Either Text a)
toByteStringParser (ValueDecoder _ _ _ decoder) = Binary.valueParser decoder
