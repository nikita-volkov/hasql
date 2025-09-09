module Hasql.Decoders.Value where

import Hasql.PostgresTypeInfo qualified as PTI
import Hasql.Prelude
import PostgreSQL.Binary.Decoding qualified as A

data Value a
  = Value
      -- | Type name.
      Text
      -- | Statically known OID for the type.
      (Maybe PTI.OID)
      -- | Statically known OID for the array-type with this type as the element.
      (Maybe PTI.OID)
      -- | Decoding function for float timestamps (integerDatetimes = False).
      (A.Value a)
      -- | Decoding function for integer timestamps (integerDatetimes = True).
      (A.Value a)
  deriving (Functor)

instance Filterable Value where
  {-# INLINE mapMaybe #-}
  mapMaybe fn =
    refine (maybe (Left "Invalid value") Right . fn)

{-# INLINE run #-}
run :: Value a -> Bool -> A.Value a
run (Value _ _ _ floatDecoder intDecoder) integerDatetimes =
  if integerDatetimes then intDecoder else floatDecoder

{-# INLINE decoder #-}
decoder :: A.Value a -> Value a
decoder aDecoder =
  {-# SCC "decoder" #-}
  Value "unknown" Nothing Nothing aDecoder aDecoder

{-# INLINE decoderFn #-}
decoderFn :: (Bool -> ByteString -> Either Text a) -> Value a
decoderFn fn =
  Value "unknown" Nothing Nothing 
    (A.fn $ fn False) 
    (A.fn $ fn True)

-- |
-- Refine a value decoder, lifting the possible error to the session level.
{-# INLINE refine #-}
refine :: (a -> Either Text b) -> Value a -> Value b
refine fn (Value typeName typeOID arrayOID floatDecoder intDecoder) = 
  Value typeName typeOID arrayOID (A.refine fn floatDecoder) (A.refine fn intDecoder)

-- |
-- Create a decoder from PTI metadata and a decoding function.
{-# INLINE unsafePTI #-}
unsafePTI :: Text -> PTI.PTI -> A.Value a -> A.Value a -> Value a
unsafePTI typeName pti floatDecoder intDecoder =
  Value typeName (Just (PTI.ptiOID pti)) (PTI.ptiArrayOID pti) floatDecoder intDecoder
