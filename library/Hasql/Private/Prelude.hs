module Hasql.Private.Prelude
(
  module Exports,
  LazyByteString,
  ByteStringBuilder,
  LazyText,
  TextBuilder,
  bug,
  bottom,
  forMToZero_,
  forMFromZero_,
  strictCons,
  mapLeft,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding (assert, left, right, isLeft, isRight, fromLeft, fromRight, error, (<>), First(..), Last(..), new)

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
import Data.Functor.Compose as Exports
import Data.Functor.Identity as Exports

-- mtl
-------------------------
import Control.Monad.Error.Class as Exports (MonadError (..))

-- profunctors
-------------------------
import Data.Profunctor.Unsafe as Exports

-- contravariant
-------------------------
import Data.Functor.Contravariant as Exports
import Data.Functor.Contravariant.Divisible as Exports

-- contravariant-extras
-------------------------
import Contravariant.Extras as Exports

-- semigroups
-------------------------
import Data.Semigroup as Exports

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

-- dlist
-------------------------
import Data.DList as Exports (DList)

-- placeholders
-------------------------
import Development.Placeholders as Exports

-- loch-th
-------------------------
import Debug.Trace.LocationTH as Exports

-- postgresql-binary
-------------------------
import PostgreSQL.Binary.Data as Exports (UUID)

-- custom
-------------------------
import qualified Debug.Trace.LocationTH
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Builder


type LazyByteString =
  Data.ByteString.Lazy.ByteString

type ByteStringBuilder =
  Data.ByteString.Builder.Builder

type LazyText =
  Data.Text.Lazy.Text

type TextBuilder =
  Data.Text.Lazy.Builder.Builder

bug =
  [e| $(Debug.Trace.LocationTH.failure) . (msg <>) |]
  where
    msg = "A \"hasql\" package bug: " :: String

bottom =
  [e| $bug "Bottom evaluated" |]

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

{-# INLINE mapLeft #-}
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f =
  either (Left . f) Right
