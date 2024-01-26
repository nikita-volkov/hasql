module Hasql.Decoders.Value where

import Hasql.Prelude
import qualified PostgreSQL.Binary.Decoding as A

newtype Value a
  = Value (Bool -> A.Value a)
  deriving (Functor)

{-# INLINE run #-}
run :: Value a -> Bool -> A.Value a
run (Value imp) integerDatetimes =
  imp integerDatetimes

{-# INLINE decoder #-}
decoder :: (Bool -> A.Value a) -> Value a
decoder =
  {-# SCC "decoder" #-}
  Value

{-# INLINE decoderFn #-}
decoderFn :: (Bool -> ByteString -> Either Text a) -> Value a
decoderFn fn =
  Value $ \integerDatetimes -> A.fn $ fn integerDatetimes
