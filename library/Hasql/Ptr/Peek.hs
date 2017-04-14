module Hasql.Ptr.Peek
(
  run,
  Peek,
  word8,
  beWord32,
  leWord32,
  bytes,
)
where

import Hasql.Prelude
import qualified Hasql.Ptr.IO as A


{-# INLINE run #-}
run :: Peek peeked -> (Int -> (Ptr Word8 -> IO peeked) -> result) -> result
run (Peek amount ptrIO) cont =
  cont amount ptrIO

{-|
A parser with a statically known required input size.

It's the user's responsiblity to ensure that the pointer
that it reads from is filled with the required amount of bytes.
-}
data Peek result =
  {-|
  * Size of the input to be consumed
  * Action on the input pointer
  -}
  Peek !Int !(Ptr Word8 -> IO result)

deriving instance Functor Peek

instance Applicative Peek where
  {-# INLINE pure #-}
  pure x =
    Peek 0 (const (pure x))
  {-# INLINE (<*>) #-}
  (<*>) (Peek leftSize leftIO) (Peek rightSize rightIO) =
    Peek (leftSize + rightSize) io
    where
      io ptr =
        leftIO ptr <*> rightIO (plusPtr ptr leftSize)


{-# INLINE word8 #-}
word8 :: Peek Word8
word8 =
  Peek 1 A.peekWord8

{-# INLINE beWord32 #-}
beWord32 :: Peek Word32
beWord32 =
  Peek 4 A.peekBEWord32

{-# INLINE leWord32 #-}
leWord32 :: Peek Word32
leWord32 =
  Peek 4 A.peekLEWord32

{-# INLINE bytes #-}
bytes :: Int -> Peek ByteString
bytes amount =
  Peek amount (A.peekBytes amount)
