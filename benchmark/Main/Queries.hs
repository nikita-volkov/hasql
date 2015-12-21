module Main.Queries where

import Main.Prelude
import qualified Hasql.Query as HQ
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD


select1 :: Int -> HQ.Query () (Vector Int64)
select1 amount =
  {-# SCC "select1" #-} 
  HQ.statement sql mempty decoder True
  where
    !sql =
      "values " <>
      mconcat (intersperse ", " (replicate amount "(1)"))
    decoder =
      HD.rowsVector (HD.value HD.int8)

select4 :: Int -> HQ.Query () (Vector (Int64, Int64, Int64, Int64))
select4 amount =
  {-# SCC "select4" #-} 
  HQ.statement sql mempty decoder True
  where
    !sql =
      "values " <>
      mconcat (intersperse ", " (replicate amount "(1, 2, 3, 4)"))
    decoder =
      HD.rowsVector ((,,,) <$> HD.value HD.int8 <*> HD.value HD.int8 <*> HD.value HD.int8 <*> HD.value HD.int8)
