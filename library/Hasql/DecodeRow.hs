module Hasql.DecodeRow where

import Hasql.Prelude
import Hasql.Model
import qualified Hasql.ParseDataRow as A
import qualified Hasql.DecodePrimitive as B


newtype DecodeRow row =
  DecodeRow (ReaderT Bool A.ParseDataRow row)
  deriving (Functor, Applicative)

primitive :: B.DecodePrimitive column -> DecodeRow column
primitive (B.DecodePrimitive (ReaderT parser)) =
  DecodeRow (ReaderT (A.column . parser))
