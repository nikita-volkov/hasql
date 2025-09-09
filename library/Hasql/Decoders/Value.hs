module Hasql.Decoders.Value where

import Hasql.Prelude
import Hasql.PostgresTypeInfo qualified as PTI
import PostgreSQL.Binary.Decoding qualified as A

data Value a
  = Value
      -- | Type name.
      Text
      -- | Statically known OID for the type.
      (Maybe PTI.OID)
      -- | Statically known OID for the array-type with this type as the element.
      (Maybe PTI.OID)
      -- | Decoding function.
      (Bool -> A.Value a)
  deriving (Functor)

instance Filterable Value where
  {-# INLINE mapMaybe #-}
  mapMaybe fn =
    refine (maybe (Left "Invalid value") Right . fn)

{-# INLINE run #-}
run :: Value a -> Bool -> A.Value a
run (Value _ _ _ imp) integerDatetimes =
  imp integerDatetimes

{-# INLINE decoder #-}
decoder :: (Bool -> A.Value a) -> Value a
decoder =
  {-# SCC "decoder" #-}
  Value "unknown" Nothing Nothing

{-# INLINE decoderFn #-}
decoderFn :: (Bool -> ByteString -> Either Text a) -> Value a
decoderFn fn =
  Value "unknown" Nothing Nothing $ \integerDatetimes -> A.fn $ fn integerDatetimes

-- |
-- Refine a value decoder, lifting the possible error to the session level.
{-# INLINE refine #-}
refine :: (a -> Either Text b) -> Value a -> Value b
refine fn (Value typeName typeOID arrayOID run) = Value typeName typeOID arrayOID (A.refine fn . run)

-- |
-- Create a decoder from PTI metadata and a decoding function.
{-# INLINE unsafePTI #-}
unsafePTI :: Text -> PTI.PTI -> (Bool -> A.Value a) -> Value a
unsafePTI typeName pti decodingFn =
  Value typeName (Just (PTI.ptiOID pti)) (PTI.ptiArrayOID pti) decodingFn
