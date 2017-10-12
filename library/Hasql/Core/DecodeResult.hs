module Hasql.Core.DecodeResult where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.InterpretResponses as A
import qualified Hasql.Core.DecodeRow as B
import qualified Control.Foldl as C
import qualified Data.HashMap.Strict as L


newtype DecodeResult result =
  DecodeResult (ReaderT Bool A.InterpretResponses result)
  deriving (Functor)

{-|
Ignore the result.
-}
{-# INLINE ignore #-}
ignore :: DecodeResult ()
ignore =
  DecodeResult (ReaderT (const (pure ())))

{-|
The amount of rows affected by the statement.
All statements produce this result.
-}
{-# INLINE length #-}
length :: DecodeResult Int
length =
  DecodeResult (ReaderT (const (A.rowsAffected)))

{-|
First row of a non-empty result.
Raises a connection error if there's no rows.
-}
{-# INLINE head #-}
head :: B.DecodeRow row -> DecodeResult row
head (B.DecodeRow (ReaderT parseDataRow)) =
  DecodeResult (ReaderT (\idt -> A.singleRow (parseDataRow idt)))

{-|
First row of a possibly empty result set.
-}
{-# INLINE headIfExists #-}
headIfExists :: B.DecodeRow row -> DecodeResult (Maybe row)
headIfExists =
  fmap fst . foldRows C.head

{-|
Vector of rows.
-}
{-# INLINE vector #-}
vector :: B.DecodeRow row -> DecodeResult (Vector row)
vector =
  fmap fst . foldRows C.vector

{-|
List of rows. Slower than 'revList'.
-}
{-# INLINE list #-}
list :: B.DecodeRow row -> DecodeResult [row]
list =
  fmap fst . foldRows C.list

{-|
List of rows in a reverse order. Faster than 'list'.
-}
{-# INLINE revList #-}
revList :: B.DecodeRow row -> DecodeResult [row]
revList =
  fmap fst . foldRows C.revList

{-|
Rows folded into a map.
-}
{-# INLINE hashMap #-}
hashMap :: (Eq key, Hashable key) => B.DecodeRow (key, value) -> DecodeResult (HashMap key value)
hashMap decodeRow =
  fmap fst (foldRows (C.Fold (\m (k, v) -> L.insert k v m) L.empty id) decodeRow)

{-|
Essentially, a specification of Map/Reduce over all the rows of the result set.
Can be used to produce all kinds of containers or to implement aggregation algorithms on the client side.

Besides the result of folding it returns the amount of affected rows,
since it's provided by the database either way.
-}
{-# INLINE foldRows #-}
foldRows :: Fold row result -> B.DecodeRow row -> DecodeResult (result, Int)
foldRows fold =
  foldMRows (C.generalize fold)

{-|
Essentially, a specification of Map/Reduce over all the rows of the result set.
Can be used to produce all kinds of containers or to implement aggregation algorithms on the client side.

Besides the result of folding it returns the amount of affected rows,
since it's provided by the database either way.
-}
{-# INLINE foldMRows #-}
foldMRows :: FoldM IO row result -> B.DecodeRow row -> DecodeResult (result, Int)
foldMRows fold (B.DecodeRow (ReaderT parseDataRow)) =
  DecodeResult (ReaderT (\idt -> A.foldRows fold (parseDataRow idt)))
