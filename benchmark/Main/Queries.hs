module Main.Queries where

import Main.Prelude
import qualified Hasql as H
import qualified Hasql.Encoding as HE
import qualified Hasql.Decoding as HD


select1 :: Int -> H.Query () (Vector Int64)
select1 amount =
  {-# SCC "select1" #-} 
  (sql, mempty, decoder, True)
  where
    !sql =
      "values " <>
      mconcat (intersperse ", " (replicate amount "(1)"))
    decoder =
      HD.rowsVector (HD.value HD.int8)

select4 :: Int -> H.Query () (Vector (Int64, Int64, Int64, Int64))
select4 amount =
  {-# SCC "select4" #-} 
  (sql, mempty, decoder, True)
  where
    !sql =
      "values " <>
      mconcat (intersperse ", " (replicate amount "(1, 2, 3, 4)"))
    decoder =
      HD.rowsVector ((,,,) <$> HD.value HD.int8 <*> HD.value HD.int8 <*> HD.value HD.int8 <*> HD.value HD.int8)
