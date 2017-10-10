module Hasql.DecodeResult where

import Hasql.Prelude
import Hasql.Model
import qualified Hasql.InterpretResponses as A
import qualified Hasql.DecodeRow as B


newtype DecodeResult result =
  DecodeResult (ReaderT Bool A.InterpretResponses result)
  deriving (Functor)

rowsAffected :: DecodeResult Int
rowsAffected =
  DecodeResult (ReaderT (const (A.rowsAffected)))

rows :: FoldM IO row result -> B.DecodeRow row -> DecodeResult (result, Int)
rows fold (B.DecodeRow (ReaderT parseDataRow)) =
  DecodeResult (ReaderT (\idt -> A.foldRows fold (parseDataRow idt)))

{-|
Exactly one row
-}
row :: B.DecodeRow row -> DecodeResult row
row (B.DecodeRow (ReaderT parseDataRow)) =
  DecodeResult (ReaderT (\idt -> A.singleRow (parseDataRow idt)))

unit :: DecodeResult ()
unit =
  DecodeResult (ReaderT (const (pure ())))
