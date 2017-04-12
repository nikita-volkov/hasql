{-# LANGUAGE CPP #-}
module Hasql.Ptr.Do
(
  run,
  Do,
  peekStorable,
  peekWord8,
  peekBEWord16,
  peekLEWord16,
  peekBEWord32,
  peekLEWord32,
  peekBEWord64,
  peekLEWord64,
  peekBytes,
)
where

import Hasql.Prelude
import qualified Data.ByteString.Internal as A


run :: Do result -> Ptr Word8 -> IO result
run (Do (ReaderT def)) =
  def

{-|
Act on a pointer.
-}
newtype Do result =
  Do (ReaderT (Ptr Word8) IO result)
  deriving (Functor, Applicative, Monad, MonadIO)

{-# INLINE io #-}
io :: (Ptr Word8 -> IO result) -> Do result
io def =
  Do (ReaderT (\receiver -> def receiver))

-- |
-- Storable value of the given amount of bytes.
{-# INLINE peekStorable #-}
peekStorable :: Storable storable => Do storable
peekStorable =
  io $ \ptr -> peek (castPtr ptr)

{-# INLINE peekWord8 #-}
peekWord8 :: Do Word8
peekWord8 =
  peekStorable

-- | Big-endian word of 2 bytes.
{-# INLINE peekBEWord16 #-}
peekBEWord16 :: Do Word16
#ifdef WORDS_BIGENDIAN
peekBEWord16 =
  peekStorable
#else
peekBEWord16 =
  byteSwap16 <$> peekStorable
#endif

-- | Little-endian word of 2 bytes.
{-# INLINE peekLEWord16 #-}
peekLEWord16 :: Do Word16
#ifdef WORDS_BIGENDIAN
peekLEWord16 =
  byteSwap16 <$> peekStorable
#else
peekLEWord16 =
  peekStorable
#endif

-- | Big-endian word of 4 bytes.
{-# INLINE peekBEWord32 #-}
peekBEWord32 :: Do Word32
#ifdef WORDS_BIGENDIAN
peekBEWord32 =
  peekStorable
#else
peekBEWord32 =
  byteSwap32 <$> peekStorable
#endif

-- | Little-endian word of 4 bytes.
{-# INLINE peekLEWord32 #-}
peekLEWord32 :: Do Word32
#ifdef WORDS_BIGENDIAN
peekLEWord32 =
  byteSwap32 <$> peekStorable
#else
peekLEWord32 =
  peekStorable
#endif

-- | Big-endian word of 8 bytes.
{-# INLINE peekBEWord64 #-}
peekBEWord64 :: Do Word64
#ifdef WORDS_BIGENDIAN
peekBEWord64 =
  peekStorable
#else
peekBEWord64 =
  byteSwap64 <$> peekStorable
#endif

-- | Little-endian word of 8 bytes.
{-# INLINE peekLEWord64 #-}
peekLEWord64 :: Do Word64
#ifdef WORDS_BIGENDIAN
peekLEWord64 =
  byteSwap64 <$> peekStorable
#else
peekLEWord64 =
  peekStorable
#endif

{-|
Allocate a new byte array with @memcpy@.
-}
{-# INLINE peekBytes #-}
peekBytes :: Int -> Do ByteString
peekBytes amount =
  io $ \ptr -> A.create amount $ \destPtr -> A.memcpy destPtr ptr amount
