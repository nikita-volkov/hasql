module Main.Queries where

import Main.Prelude
import qualified Hasql as H
import qualified Hasql.Serialization as HS
import qualified Hasql.Deserialization as HD


select1 :: Int -> H.Query () ([] Int64)
select1 amount =
  {-# SCC "select1" #-} 
  (sql, mempty, deserializer, True)
  where
    !sql =
      "values " <>
      mconcat (intersperse ", " (replicate amount "(1)"))
    deserializer =
      HD.rowsList (HD.value HD.int8)

select4 :: Int -> H.Query () ([] (Int64, Int64, Int64, Int64))
select4 amount =
  {-# SCC "select4" #-} 
  (sql, mempty, deserializer, True)
  where
    !sql =
      "values " <>
      mconcat (intersperse ", " (replicate amount "(1, 2, 3, 4)"))
    deserializer =
      HD.rowsList ((,,,) <$> HD.value HD.int8 <*> HD.value HD.int8 <*> HD.value HD.int8 <*> HD.value HD.int8)
