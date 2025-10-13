module Codecs.Decoders.Value where

import Codecs.TypeInfo qualified as TypeInfo
import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as Binary

-- |
-- Value decoder.
data Value a
  = Value
      -- | Type name.
      Text
      -- | Statically known OID for the type.
      (Maybe Word32)
      -- | Statically known OID for the array-type with this type as the element.
      (Maybe Word32)
      -- | Decoding function (always integer timestamps for PostgreSQL 10+).
      (Binary.Value a)
  deriving (Functor)

instance Filterable Value where
  {-# INLINE mapMaybe #-}
  mapMaybe fn =
    refine (maybe (Left "Invalid value") Right . fn)

{-# INLINE decoder #-}
decoder :: Binary.Value a -> Value a
decoder aDecoder =
  {-# SCC "decoder" #-}
  Value "unknown" Nothing Nothing aDecoder

{-# INLINE decoderFn #-}
decoderFn :: (Bool -> ByteString -> Either Text a) -> Value a
decoderFn fn =
  Value
    "unknown"
    Nothing
    Nothing
    (Binary.fn $ fn True) -- Always use integer timestamps

-- |
-- Refine a value decoder, lifting the possible error to the session level.
{-# INLINE refine #-}
refine :: (a -> Either Text b) -> Value a -> Value b
refine fn (Value typeName typeOid arrayOid decoder) =
  Value typeName typeOid arrayOid (Binary.refine fn decoder)

-- |
-- Create a decoder from TypeInfo metadata and a decoding function.
{-# INLINE unsafeTypeInfo #-}
unsafeTypeInfo :: Text -> TypeInfo.TypeInfo -> Binary.Value a -> Value a
unsafeTypeInfo typeName pti intDecoder =
  Value typeName (Just (TypeInfo.toBaseOid pti)) (Just (TypeInfo.toArrayOid pti)) intDecoder

-- * Relations

toTypeName :: Value a -> Text
toTypeName (Value typeName _ _ _) = typeName

toBaseOid :: Value a -> Maybe Word32
toBaseOid (Value _ typeOid _ _) =
  typeOid

toArrayOid :: Value a -> Maybe Word32
toArrayOid (Value _ _ oid _) = oid

{-# INLINE toHandler #-}
toHandler :: Value a -> Binary.Value a
toHandler (Value _ _ _ decoder) = decoder

{-# INLINE toByteStringParser #-}
toByteStringParser :: Value a -> (ByteString -> Either Text a)
toByteStringParser (Value _ _ _ decoder) = Binary.valueParser decoder
