module Hasql.Private.Decoders.Value where

import Hasql.Private.Prelude
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified PostgreSQL.Binary.Decoder as Decoder


newtype Value a =
  Value (ReaderT Bool Decoder.Decoder a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)


{-# INLINE run #-}
run :: Value a -> Bool -> Decoder.Decoder a
run (Value imp) integerDatetimes =
  runReaderT imp integerDatetimes

{-# INLINE decoder #-}
decoder :: (Bool -> Decoder.Decoder a) -> Value a
decoder =
  {-# SCC "decoder" #-}
  Value . ReaderT

{-# INLINE decoderFn #-}
decoderFn :: (Bool -> ByteString -> Either Text a) -> Value a
decoderFn fn =
  Value $ ReaderT $ \integerDatetimes -> Decoder.fn $ fn integerDatetimes
