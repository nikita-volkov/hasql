module Hasql.DecodeResult where

import Hasql.Prelude
import Hasql.Model
import qualified Hasql.ParseMessageStream as A
import qualified Hasql.DecodeRow as B


newtype DecodeResult result =
  DecodeResult (ReaderT Bool A.ParseMessageStream result)
  deriving (Functor)

rowsAffected :: DecodeResult Int
rowsAffected =
  DecodeResult (ReaderT (const (A.rowsAffected)))

rows :: Fold row result -> B.DecodeRow row -> DecodeResult result
rows fold (B.DecodeRow (ReaderT parseDataRow)) =
  DecodeResult (ReaderT (\idt -> A.rows (parseDataRow idt) fold))

{-|
Exactly one row
-}
row :: B.DecodeRow row -> DecodeResult row
row (B.DecodeRow (ReaderT parseDataRow)) =
  DecodeResult (ReaderT (\idt -> A.row (parseDataRow idt)))

unit :: DecodeResult ()
unit =
  DecodeResult (ReaderT (const (pure ())))
