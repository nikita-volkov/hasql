module Hasql.Buffer where

import Hasql.Prelude hiding (State, Buffer)
import Foreign.C


foreign import ccall unsafe "memmove"
  memmove :: Ptr a {-^ Destination -} -> Ptr a {-^ Source -} -> CSize {-^ Count -} -> IO (Ptr a) {-^ Destination -}


data Buffer =
  Buffer !(ForeignPtr Word8) !(IORef State)

data State =
  {-|
  * Start offset
  * End offset
  * Size
  -}
  State !Int !Int !Int

new :: Int -> IO Buffer
new space =
  Buffer <$> mallocForeignPtrBytes space <*> newIORef (State 0 0 0)

getRemainingSpace :: Buffer -> IO Int
getRemainingSpace (Buffer _ stateRef) =
  do
    State start end size <- readIORef stateRef
    return (size - end)

{-|
Ensure that the buffer has at least the specified amount of elements.
-}
demandSpace :: Buffer -> Int -> IO ()
demandSpace (Buffer fptr stateRef) space =
  do
    State start end size <- readIORef stateRef
    let
      remainingSpace = size - end
      delta = space - remainingSpace
      occupiedSpace = end - start
      in if delta <= 0
        then
          return ()
        else 
          if delta > start
            then do
              -- Grow
              traceEventIO ("START Buffer/Grow")
              $(todo "Grow")
            else do
              traceEventIO ("START Buffer/Align")
              withForeignPtr fptr (\ptr -> memmove ptr (plusPtr ptr start) (fromIntegral occupiedSpace))
              traceEventIO ("STOP Buffer/Align")
              writeIORef stateRef (State 0 occupiedSpace size)

{-|
It is the user's responsibility that the pointer action
does not exceed the limits.
-}
fill :: Buffer -> (Int -> Ptr Word8 -> IO Int) -> IO ()
fill (Buffer fptr stateRef) =
  $(todo "")
