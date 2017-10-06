module Hasql.Loops.Serializer where

import Hasql.Prelude
import qualified PtrMagic.Encoding as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as C


data Message =
  SerializeMessage !A.Encoding |
  FlushMessage

loop :: IO Message -> (ByteString -> IO ()) -> IO ()
loop getMessage sendBytes =
  startAnew
  where
    size =
      shiftL 2 12
    startAnew =
      do
        fp <- mallocForeignPtrBytes size
        processNextMessage fp 0
    processNextMessage !fp !offset =
      do
        message <- getMessage
        case message of
          SerializeMessage (A.Encoding spaceRequired write) ->
            serialize fp offset spaceRequired write
          FlushMessage ->
            do
              sendBytes (C.PS fp 0 offset)
              startAnew
    serialize !fp !offset !spaceRequired !write =
      if size - offset >= spaceRequired
        then do
          withForeignPtr fp (\p -> write (plusPtr p offset))
          processNextMessage fp (offset + spaceRequired)
        else do
          when (offset >= 0) (sendBytes (C.PS fp 0 offset))
          if spaceRequired >= size
            then do
              newFP <- mallocForeignPtrBytes spaceRequired
              withForeignPtr newFP write
              sendBytes (C.PS newFP 0 spaceRequired)
              startAnew
            else do
              newFP <- mallocForeignPtrBytes size
              withForeignPtr newFP write
              processNextMessage newFP spaceRequired
        
