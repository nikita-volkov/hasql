module Hasql.Prelude
(
  module Exports,
  forMToZero_,
  forMFromZero_,
  strictCons,
  regions,
  traceEventIO,
  traceEvent,
  traceMarkerIO,
  traceMarker,
  startThread,
  startThreads,
  ErrorWithContext(..),
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding (assert, left, right, isLeft, isRight, (<>), First(..), Last(..), ProtocolError, traceEvent, traceEventIO, traceMarker, traceMarkerIO)

-- transformers
-------------------------
import Control.Monad.IO.Class as Exports
import Control.Monad.Trans.Class as Exports
import Control.Monad.Trans.Cont as Exports hiding (shift, callCC)
import Control.Monad.Trans.Except as Exports (ExceptT(ExceptT), Except, except, runExcept, runExceptT, mapExcept, mapExceptT, withExcept, withExceptT, throwE, catchE)
import Control.Monad.Trans.Maybe as Exports
import Control.Monad.Trans.Reader as Exports (Reader, runReader, mapReader, withReader, ReaderT(ReaderT), runReaderT, mapReaderT, withReaderT)
import Control.Monad.Trans.State.Strict as Exports (State, runState, evalState, execState, mapState, withState, StateT(StateT), runStateT, evalStateT, execStateT, mapStateT, withStateT)
import Control.Monad.Trans.Writer.Strict as Exports (Writer, runWriter, execWriter, mapWriter, WriterT(..), execWriterT, mapWriterT)

-- mtl
-------------------------
import Control.Monad.Cont.Class as Exports
import Control.Monad.Error.Class as Exports hiding (Error(..))
import Control.Monad.Reader.Class as Exports
import Control.Monad.State.Class as Exports
import Control.Monad.Writer.Class as Exports

-- contravariant
-------------------------
import Data.Functor.Contravariant as Exports
import Data.Functor.Contravariant.Divisible as Exports

-- profunctors
-------------------------
import Data.Profunctor.Unsafe as Exports

-- semigroups
-------------------------
import Data.Semigroup as Exports

-- foldl
-------------------------
import Control.Foldl as Exports (Fold(..), FoldM(..))

-- stm
-------------------------
import Control.Concurrent.STM as Exports

-- hashable
-------------------------
import Data.Hashable as Exports (Hashable(..))

-- text
-------------------------
import Data.Text as Exports (Text)

-- bytestring
-------------------------
import Data.ByteString as Exports (ByteString)

-- vector
-------------------------
import Data.Vector as Exports (Vector)

-- containers
-------------------------
import Data.IntMap.Strict as Exports (IntMap)
import Data.IntSet as Exports (IntSet)
import Data.Map.Strict as Exports (Map)
import Data.Sequence as Exports (Seq)
import Data.Set as Exports (Set)

-- unordered-containers
-------------------------
import Data.HashSet as Exports (HashSet)
import Data.HashMap.Strict as Exports (HashMap)

-- dlist
-------------------------
import Data.DList as Exports (DList)

-- bug
-------------------------
import Bug as Exports

-- 
-------------------------

import qualified GHC.RTS.Flags as A
import qualified BasePrelude as B


-- * Workarounds for unremoved event logging
-------------------------

{-# NOINLINE matchTraceUserEvents #-}
matchTraceUserEvents :: a -> a -> a
matchTraceUserEvents =
  case A.user (unsafeDupablePerformIO A.getTraceFlags) of
    True -> \_ x -> x
    False -> \x _ -> x

{-# NOINLINE traceEventIO #-}
!traceEventIO =
  matchTraceUserEvents (const (return ())) B.traceEventIO

{-# NOINLINE traceEvent #-}
!traceEvent =
  matchTraceUserEvents (const id) B.traceEvent

{-# NOINLINE traceMarkerIO #-}
!traceMarkerIO =
  matchTraceUserEvents (const (return ())) B.traceMarkerIO

{-# NOINLINE traceMarker #-}
!traceMarker =
  matchTraceUserEvents (const id) B.traceMarker

{-# INLINE forMToZero_ #-}
forMToZero_ :: Applicative m => Int -> (Int -> m a) -> m ()
forMToZero_ !startN f =
  ($ pred startN) $ fix $ \loop !n -> if n >= 0 then f n *> loop (pred n) else pure ()

{-# INLINE forMFromZero_ #-}
forMFromZero_ :: Applicative m => Int -> (Int -> m a) -> m ()
forMFromZero_ !endN f =
  ($ 0) $ fix $ \loop !n -> if n < endN then f n *> loop (succ n) else pure ()

{-# INLINE strictCons #-}
strictCons :: a -> [a] -> [a]
strictCons !a b =
  let !c = a : b in c

{-|
An integer space distributed maximally evenly across regions.
-}
regions :: Int -> Int -> [(Int, Int)]
regions maxRegions space =
  case divMod space maxRegions of
    (baseSize, remainingSpace) ->
      build [] maxRegions space remainingSpace
      where
        build state regionsState spaceState remainingSpaceState =
          if remainingSpaceState > 0
            then
              addRegion (succ baseSize) (pred remainingSpaceState)
            else 
              if regionsState > 0 && baseSize > 0
                then
                  addRegion baseSize remainingSpaceState
                else
                  state
          where
            addRegion regionSize remainingSpaceState =
              build (region : state) (pred regionsState) regionStart remainingSpaceState
              where
                !region =
                  (regionStart, spaceState)
                !regionStart =
                  spaceState - regionSize

{-# INLINE startThread #-}
startThread :: IO () -> IO (IO ())
startThread action =
  fmap killThread (forkIO action)

{-# INLINE startThreads #-}
startThreads :: [IO ()] -> IO (IO ())
startThreads =
  fmap sequence_ . traverse startThread

data ErrorWithContext =
  ContextErrorWithContext !Text !ErrorWithContext |
  MessageErrorWithContext !Text
