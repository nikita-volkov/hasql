module Hasql.Core.Loops.Serializer where

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
    startAnew =
      do
        fp <- mallocForeignPtrBytes size
        loop fp 0 size
      where
        size = shiftL 2 13
    loop !fp !offset !size =
      do
        message <- getMessage
        case message of
          SerializeMessage (A.Encoding spaceRequired write) ->
            if size - offset >= spaceRequired
              then do
                withForeignPtr fp (\p -> write (plusPtr p offset))
                loop fp (offset + spaceRequired) size
              else do
                let
                  newMinSize = offset + spaceRequired
                  newSize = head $ dropWhile (< newMinSize) $ map (shiftL 2) $ enumFrom 0
                newFP <- mallocForeignPtrBytes newSize
                withForeignPtr newFP $ \newP -> do
                  withForeignPtr fp (\p -> C.memcpy newP p offset)
                  write (plusPtr newP offset)
                loop newFP (offset + spaceRequired) newSize
          FlushMessage ->
            do
              sendBytes (C.PS fp 0 offset)
              startAnew
