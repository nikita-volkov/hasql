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
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding (assert, left, right, isLeft, isRight, error, (<>), First(..), Last(..), new)

-- transformers
-------------------------
import Control.Monad.IO.Class as Exports
import Control.Monad.Trans.Class as Exports
import Control.Monad.Trans.Maybe as Exports hiding (liftListen, liftPass)
import Control.Monad.Trans.Reader as Exports hiding (liftCallCC, liftCatch)
import Control.Monad.Trans.State.Strict as Exports hiding (liftCallCC, liftCatch, liftListen, liftPass)
import Data.Functor.Identity as Exports

-- mtl
-------------------------
import Control.Monad.Error.Class as Exports (MonadError (..))

-- data-default-class
-------------------------
import Data.Default.Class as Exports

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

-- either
-------------------------
import Control.Monad.Trans.Either as Exports
import Data.Either.Combinators as Exports

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

-- scientific
-------------------------
import Data.Scientific as Exports (Scientific)

-- uuid
-------------------------
import Data.UUID as Exports (UUID)

-- time
-------------------------
import Data.Time as Exports

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
