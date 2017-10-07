module Hasql.DecodeRow where

import Hasql.Prelude
import Hasql.Model
import qualified Hasql.ParseDataRow as A
import qualified Hasql.DecodePrimitive as B


newtype DecodeRow row =
  DecodeRow (ReaderT Bool A.ParseDataRow row)
  deriving (Functor, Applicative)

nonNullPrimitive :: B.DecodePrimitive column -> DecodeRow column
nonNullPrimitive (B.DecodePrimitive (ReaderT parser)) =
  DecodeRow (ReaderT (A.nonNullColumn . parser))
