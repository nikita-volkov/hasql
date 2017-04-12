module Hasql.Buffer
(
  Buffer,
  new,
  put,
  take,
  getOccupiedSpace,
)
where

import Hasql.Prelude hiding (State, Buffer, put, take)
import Foreign.C


foreign import ccall unsafe "memmove"
  memmove :: Ptr a {-^ Destination -} -> Ptr a {-^ Source -} -> CSize {-^ Count -} -> IO (Ptr a) {-^ Destination -}

foreign import ccall unsafe "memcpy"
  memcpy :: Ptr a {-^ Destination -} -> Ptr a {-^ Source -} -> CSize {-^ Count -} -> IO (Ptr a) {-^ Destination -}


newtype Buffer =
  Buffer (IORef State)

data State =
  {-|
  * Buffer pointer
  * Start offset
  * End offset
  * Max amount
  -}
  State !(ForeignPtr Word8) !Int !Int !Int

new :: Int -> IO Buffer
new space =
  do
    fptr <- mallocForeignPtrBytes space
    stateIORef <- newIORef (State fptr 0 0 space)
    return (Buffer stateIORef)

{-|
Fill the buffer with the specified amount of bytes.

Aligns or grows the buffer if required.

It is the user's responsibility that the pointer action
does not exceed the limits.
-}
put :: Buffer -> Int {-^ Amount of bytes to be written -} -> (Ptr Word8 -> IO (result, Int {-^ Amount actually added -})) {-^ Poker -} -> IO result
put (Buffer stateIORef) space ptrIO =
  do
    State fptr start end boundary <- readIORef stateIORef
    let
      remainingSpace = boundary - end
      delta = space - remainingSpace
      occupiedSpace = end - start
      in if delta <= 0 -- Doesn't need more space?
        then do
          (result, addedSpace) <- withForeignPtr fptr $ \ptr -> ptrIO (plusPtr ptr end)
          writeIORef stateIORef (State fptr start (end + addedSpace) boundary)
          return result
        else if delta > start -- Needs growing?
          then do
            -- Grow
            traceEventIO ("START Buffer/Grow")
            newFPtr <- mallocForeignPtrBytes (occupiedSpace + space)
            (result, addedSpace) <-
              withForeignPtr newFPtr $ \newPtr -> do
                withForeignPtr fptr $ \ptr -> do
                  memcpy newPtr (plusPtr ptr start) (fromIntegral occupiedSpace)
                traceEventIO ("STOP Buffer/Grow")
                ptrIO (plusPtr newPtr occupiedSpace)
            let newOccupiedSpace = occupiedSpace + addedSpace
            writeIORef stateIORef (State newFPtr 0 newOccupiedSpace newOccupiedSpace)
            return result
          else if occupiedSpace > 0 -- Needs aligning?
            then do
              -- Align
              (result, addedSpace) <-
                withForeignPtr fptr $ \ptr -> do
                  traceEventIO ("START Buffer/Align")
                  memmove ptr (plusPtr ptr start) (fromIntegral occupiedSpace)
                  traceEventIO ("STOP Buffer/Align")
                  ptrIO (plusPtr ptr occupiedSpace)
              writeIORef stateIORef (State fptr 0 (occupiedSpace + addedSpace) boundary)
              return result
            else do
              (result, addedSpace) <- withForeignPtr fptr ptrIO
              writeIORef stateIORef (State fptr 0 addedSpace boundary)
              return result

take :: Buffer -> (Ptr Word8 -> Int {-^ Available amount -} -> IO (result, Int {-^ Taken amount -})) -> IO result
take (Buffer stateIORef) ptrIO =
  do
    State fptr start end boundary <- readIORef stateIORef
    withForeignPtr fptr $ \ptr -> do
      (result, amountTaken) <- ptrIO (plusPtr ptr start) (end - start)
      writeIORef stateIORef (State fptr (start + amountTaken) end boundary)
      return result

getOccupiedSpace :: Buffer -> IO Int
getOccupiedSpace (Buffer stateIORef) =
  do
    State fptr start end boundary <- readIORef stateIORef
    return (end - start)

