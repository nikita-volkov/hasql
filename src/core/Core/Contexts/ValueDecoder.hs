module Core.Contexts.ValueDecoder where

import Core.PostgresTypeInfo qualified as PTI
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
      -- | Decoding function for float timestamps (integerDatetimes = False).
      (A.Value a)
      -- | Decoding function for integer timestamps (integerDatetimes = True).
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
  ValueDecoder "unknown" Nothing Nothing aDecoder aDecoder

{-# INLINE decoderFn #-}
decoderFn :: (Bool -> ByteString -> Either Text a) -> ValueDecoder a
decoderFn fn =
  ValueDecoder
    "unknown"
    Nothing
    Nothing
    (A.fn $ fn False)
    (A.fn $ fn True)

-- |
-- Refine a value decoder, lifting the possible error to the session level.
{-# INLINE refine #-}
refine :: (a -> Either Text b) -> ValueDecoder a -> ValueDecoder b
refine fn (ValueDecoder typeName typeOID arrayOID floatDecoder intDecoder) =
  ValueDecoder typeName typeOID arrayOID (A.refine fn floatDecoder) (A.refine fn intDecoder)

-- |
-- Create a decoder from PTI metadata and a decoding function.
{-# INLINE unsafePTI #-}
unsafePTI :: Text -> PTI.PTI -> A.Value a -> A.Value a -> ValueDecoder a
unsafePTI typeName pti floatDecoder intDecoder =
  ValueDecoder typeName (Just (PTI.ptiOID pti)) (PTI.ptiArrayOID pti) floatDecoder intDecoder

-- * Relations

toExpectedOid :: ValueDecoder a -> Maybe Pq.Oid
toExpectedOid (ValueDecoder _ typeOID _ _ _) = PTI.oidPQ <$> typeOID

{-# INLINE toHandler #-}
toHandler :: ValueDecoder a -> Bool -> A.Value a
toHandler (ValueDecoder _ _ _ floatDecoder intDecoder) integerDatetimes =
  if integerDatetimes then intDecoder else floatDecoder
