{-# LANGUAGE CPP #-}
module Hasql.Ptr.IO
where

import Hasql.Prelude
import qualified Data.ByteString.Internal as A


{-# INLINE peekStorable #-}
peekStorable :: Storable storable => Ptr Word8 -> IO storable
peekStorable =
  peek . castPtr

{-# INLINE peekWord8 #-}
peekWord8 :: Ptr Word8 -> IO Word8
peekWord8 =
  peekStorable

-- | Big-endian word of 2 bytes.
{-# INLINE peekBEWord16 #-}
peekBEWord16 :: Ptr Word8 -> IO Word16
#ifdef WORDS_BIGENDIAN
peekBEWord16 =
  peekStorable
#else
peekBEWord16 =
  fmap byteSwap16 . peekStorable
#endif

-- | Little-endian word of 2 bytes.
{-# INLINE peekLEWord16 #-}
peekLEWord16 :: Ptr Word8 -> IO Word16
#ifdef WORDS_BIGENDIAN
peekLEWord16 =
  fmap byteSwap16 . peekStorable
#else
peekLEWord16 =
  peekStorable
#endif

-- | Big-endian word of 4 bytes.
{-# INLINE peekBEWord32 #-}
peekBEWord32 :: Ptr Word8 -> IO Word32
#ifdef WORDS_BIGENDIAN
peekBEWord32 =
  peekStorable
#else
peekBEWord32 =
  fmap byteSwap32 . peekStorable
#endif

-- | Little-endian word of 4 bytes.
{-# INLINE peekLEWord32 #-}
peekLEWord32 :: Ptr Word8 -> IO Word32
#ifdef WORDS_BIGENDIAN
peekLEWord32 =
  fmap byteSwap32 . peekStorable
#else
peekLEWord32 =
  peekStorable
#endif

-- | Big-endian word of 8 bytes.
{-# INLINE peekBEWord64 #-}
peekBEWord64 :: Ptr Word8 -> IO Word64
#ifdef WORDS_BIGENDIAN
peekBEWord64 =
  peekStorable
#else
peekBEWord64 =
  fmap byteSwap64 . peekStorable
#endif

-- | Little-endian word of 8 bytes.
{-# INLINE peekLEWord64 #-}
peekLEWord64 :: Ptr Word8 -> IO Word64
#ifdef WORDS_BIGENDIAN
peekLEWord64 =
  fmap byteSwap64 . peekStorable
#else
peekLEWord64 =
  peekStorable
#endif

{-|
Allocate a new byte array with @memcpy@.
-}
{-# INLINE peekBytes #-}
peekBytes :: Int -> Ptr Word8 -> IO ByteString
peekBytes amount ptr =
  A.create amount $ \destPtr -> A.memcpy destPtr ptr amount
