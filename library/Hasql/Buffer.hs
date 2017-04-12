module Hasql.Buffer
(
  Buffer,
  new,
  put,
  take,
)
where

import Hasql.Prelude hiding (State, Buffer, put, take)
import Foreign.C


foreign import ccall unsafe "memmove"
  memmove :: Ptr a {-^ Destination -} -> Ptr a {-^ Source -} -> CSize {-^ Count -} -> IO (Ptr a) {-^ Destination -}

foreign import ccall unsafe "memcpy"
  memcpy :: Ptr a {-^ Destination -} -> Ptr a {-^ Source -} -> CSize {-^ Count -} -> IO (Ptr a) {-^ Destination -}


newtype Buffer =
  Buffer (MVar State)

data State =
  {-|
  * Buffer pointer
  * Start offset
  * End offset
  * Size
  -}
  State !(ForeignPtr Word8) !Int !Int !Int

new :: Int -> IO Buffer
new space =
  do
    fptr <- mallocForeignPtrBytes space
    stateMVar <- newMVar (State fptr 0 0 space)
    return (Buffer stateMVar)

{-|
Fill the buffer with the specified amount of bytes.

Aligns or grows the buffer if required.

It is the user's responsibility that the pointer action
does not exceed the limits.
-}
put :: Buffer -> Int {-^ Amount of bytes to be written -} -> (Ptr Word8 -> IO ()) {-^ Poker -} -> IO ()
put (Buffer stateMVar) space ptrIO =
  do
    State fptr start end boundary <- takeMVar stateMVar
    let
      remainingSpace = boundary - end
      delta = space - remainingSpace
      occupiedSpace = end - start
      in if delta <= 0 -- Needs more space?
        then do
          withForeignPtr fptr $ \ptr -> do
            ptrIO (plusPtr ptr end)
          putMVar stateMVar (State fptr start (end + space) boundary)
        else if delta > start -- Needs growing?
          then do
            -- Grow
            let
              newOccupiedSpace = occupiedSpace + space
            traceEventIO ("START Buffer/Grow")
            newFPtr <- mallocForeignPtrBytes newOccupiedSpace
            withForeignPtr newFPtr $ \newPtr -> do
              withForeignPtr fptr $ \ptr -> do
                memcpy newPtr (plusPtr ptr start) (fromIntegral occupiedSpace)
              traceEventIO ("STOP Buffer/Grow")
              ptrIO (plusPtr newPtr occupiedSpace)
            putMVar stateMVar (State newFPtr 0 newOccupiedSpace newOccupiedSpace)
          else if occupiedSpace > 0 -- Needs aligning?
            then do
              -- Align
              withForeignPtr fptr $ \ptr -> do
                traceEventIO ("START Buffer/Align")
                memmove ptr (plusPtr ptr start) (fromIntegral occupiedSpace)
                traceEventIO ("STOP Buffer/Align")
                ptrIO (plusPtr ptr occupiedSpace)
              putMVar stateMVar (State fptr 0 (occupiedSpace + space) boundary)
            else do
              withForeignPtr fptr ptrIO
              putMVar stateMVar (State fptr 0 space boundary)

take :: Buffer -> (Ptr Word8 -> Int {-^ Available amount -} -> IO (result, Int {-^ Taken amount -})) -> IO result
take (Buffer stateMVar) ptrIO =
  do
    State fptr start end boundary <- takeMVar stateMVar
    withForeignPtr fptr $ \ptr -> do
      (result, amountTaken) <- ptrIO (plusPtr ptr start) (end - start)
      putMVar stateMVar (State fptr (start + amountTaken) end boundary)
      return result
