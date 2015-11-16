module Main.Queries where

import Main.Prelude
import qualified Hasql as H
import qualified Hasql.Serialization as HS
import qualified Hasql.Deserialization as HD


select1 :: Int -> H.Query () (Vector Int64)
select1 amount =
  (sql, mempty, deserializer, True)
  where
    sql =
      "values " <>
      mconcat (intersperse ", " (replicate amount "(1)"))
    deserializer =
      HD.rowsVector (HD.value HD.int8)

select4 :: Int -> H.Query () (Vector (Int64, Int64, Int64, Int64))
select4 amount =
  (sql, mempty, deserializer, True)
  where
    sql =
      "values " <>
      mconcat (intersperse ", " (replicate amount "(1, 2, 3, 4)"))
    deserializer =
      HD.rowsVector ((,,,) <$> HD.value HD.int8 <*> HD.value HD.int8 <*> HD.value HD.int8 <*> HD.value HD.int8)
