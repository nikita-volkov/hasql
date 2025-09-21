module Core.Contexts.ValueDecoder where

import Core.PostgresTypeInfo qualified as PTI
import Hipq.ValueDecoder qualified as HipqValueDecoder
import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as A
import Pq qualified

data ValueDecoder a
  = ValueDecoder
      -- | Type name.
      Text
      -- | Statically known OID for the type.
      (Maybe PTI.OID)
      -- | Statically known OID for the array-type with this type as the element.
      (Maybe PTI.OID)
      -- | Decoding function (always integer timestamps for PostgreSQL 10+).
      (A.Value a)
  deriving (Functor)

instance Filterable ValueDecoder where
  {-# INLINE mapMaybe #-}
  mapMaybe fn =
    refine (maybe (Left "Invalid value") Right . fn)

{-# INLINE decoder #-}
decoder :: A.Value a -> ValueDecoder a
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
    (A.fn $ fn True) -- Always use integer timestamps

-- |
-- Refine a value decoder, lifting the possible error to the session level.
{-# INLINE refine #-}
refine :: (a -> Either Text b) -> ValueDecoder a -> ValueDecoder b
refine fn (ValueDecoder typeName typeOID arrayOID decoder) =
  ValueDecoder typeName typeOID arrayOID (A.refine fn decoder)

-- |
-- Create a decoder from PTI metadata and a decoding function.
{-# INLINE unsafePTI #-}
unsafePTI :: Text -> PTI.PTI -> A.Value a -> ValueDecoder a
unsafePTI typeName pti intDecoder =
  ValueDecoder typeName (Just (PTI.ptiOID pti)) (PTI.ptiArrayOID pti) intDecoder

-- * Relations

toTypeName :: ValueDecoder a -> Text
toTypeName (ValueDecoder typeName _ _ _) = typeName

toBaseOid :: ValueDecoder a -> Maybe Pq.Oid
toBaseOid (ValueDecoder _ typeOID _ _) = PTI.oidPQ <$> typeOID

toBaseOidAsWord32 :: ValueDecoder a -> Maybe Word32
toBaseOidAsWord32 (ValueDecoder _ typeOID _ _) =
  PTI.oidWord32 <$> typeOID

toArrayOid :: ValueDecoder a -> Maybe PTI.OID
toArrayOid (ValueDecoder _ _ oid _) = oid

{-# INLINE toHandler #-}
toHandler :: ValueDecoder a -> A.Value a
toHandler (ValueDecoder _ _ _ decoder) = decoder

{-# INLINE toHipqValueDecoder #-}
toHipqValueDecoder :: ValueDecoder a -> HipqValueDecoder.ValueDecoder a
toHipqValueDecoder (ValueDecoder _ _ _ decoder) = HipqValueDecoder.fromParser (A.valueParser decoder)
