module Hasql.Core.DecodeRow where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.ParseDataRow as A
import qualified Hasql.Core.DecodePrimitive as B


newtype DecodeRow row =
  DecodeRow (ReaderT Bool A.ParseDataRow row)
  deriving (Functor, Applicative)

primitive :: B.DecodePrimitive column -> DecodeRow column
primitive (B.DecodePrimitive (ReaderT parser)) =
  DecodeRow (ReaderT (A.column . parser))

nullablePrimitive :: B.DecodePrimitive column -> DecodeRow (Maybe column)
nullablePrimitive (B.DecodePrimitive (ReaderT parser)) =
  DecodeRow (ReaderT (A.nullableColumn . parser))
