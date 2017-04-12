module Hasql.Protocol.Scanner where

import Hasql.Prelude
import Hasql.Protocol.Model
import Scanner (Scanner)
import qualified Scanner as A
import qualified Data.ByteString as B


{-# INLINE word8 #-}
word8 :: Scanner Word8
word8 =
  A.anyWord8

{-# INLINE word16 #-}
word16 :: Scanner Word16
word16 =
  numOfSize 2

{-# INLINE word32 #-}
word32 :: Scanner Word32
word32 =
  numOfSize 4

{-# INLINE word64 #-}
word64 :: Scanner Word64
word64 =
  numOfSize 8

{-# INLINE numOfSize #-}
numOfSize :: (Bits a, Num a) => Int -> Scanner a
numOfSize size =
  B.foldl' (\n h -> shiftL n 8 .|. fromIntegral h) 0 <$> A.take size

{-# INLINE int32 #-}
int32 :: Scanner Int32
int32 =
  fromIntegral <$> word32

{-# INLINE messageTypeAndLength #-}
messageTypeAndLength :: (MessageType -> Word32 -> a) -> Scanner a
messageTypeAndLength cont =
  cont <$> messageType <*> payloadLength

{-# INLINE messageType #-}
messageType :: Scanner MessageType
messageType =
  MessageType <$> word8

{-# INLINE payloadLength #-}
payloadLength :: (Integral a, Bits a) => Scanner a
payloadLength =
  subtract 4 <$> numOfSize 4

{-# INLINE messageTypeAndPayload #-}
messageTypeAndPayload :: (MessageType -> ByteString -> a) -> Scanner a
messageTypeAndPayload cont =
  cont <$> messageType <*> (payloadLength >>= A.take)
