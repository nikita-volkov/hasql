module Main where

import Prelude
import Bug
import Criterion
import Criterion.Main
import qualified Hasql.Connection as A
import qualified Hasql.Connection.Session as B
import qualified Hasql.Connection.Session.Statement as C
import qualified Hasql.Connection.Session.Statement.Decoding as D
import qualified Hasql.Connection.Session.Statement.Encoding as E
import qualified Data.Vector as F


main =
  do
    Right connection <- A.acquire "localhost" Nothing "postgres" Nothing Nothing
    let
      sessionBench :: NFData a => String -> B.Session a -> Benchmark
      sessionBench name session =
        bench name (nfIO (runSession session))
        where
          runSession session =
            do
              Right result <- A.use connection session
              return result
      in
        defaultMain
        [
          sessionBench "largeResultInVector" sessionWithSingleLargeResultInVector,
          sessionBench "largeResultInList" sessionWithSingleLargeResultInList,
          sessionBench "largeResultInRevList" sessionWithSingleLargeResultInRevList,
          sessionBench "manySmallResults" sessionWithManySmallResults
        ]

-- * Sessions
-------------------------

sessionWithManySmallParameters :: Vector (Int64, Int64) -> B.Session ()
sessionWithManySmallParameters =
  $(todo "sessionWithManySmallParameters")

sessionWithSingleLargeResultInVector :: B.Session (Vector (Int64, Int64))
sessionWithSingleLargeResultInVector =
  B.batch (B.statement statementWithManyRowsInVector ())

sessionWithSingleLargeResultInList :: B.Session (List (Int64, Int64))
sessionWithSingleLargeResultInList =
  B.batch (B.statement statementWithManyRowsInList ())

sessionWithSingleLargeResultInRevList :: B.Session (List (Int64, Int64))
sessionWithSingleLargeResultInRevList =
  B.batch (B.statement statementWithManyRowsInRevList ())

sessionWithManySmallResults :: B.Session (Vector (Int64, Int64))
sessionWithManySmallResults =
  F.replicateM 1000 (B.batch (B.statement statementWithSingleRow ()))


-- * Statements
-------------------------

statementWithManyParameters :: C.Statement (Vector (Int64, Int64)) ()
statementWithManyParameters =
  $(todo "statementWithManyParameters")

statementWithSingleRow :: C.Statement () (Int64, Int64)
statementWithSingleRow =
  C.statement template encoder decoder True
  where
    template =
      "SELECT 1, 2"
    encoder =
      conquer
    decoder =
      C.row row
      where
        row =
          tuple <$> C.column D.int8 <*> C.column D.int8
          where
            tuple !a !b =
              (a, b)

statementWithManyRows :: (C.RowDecoder (Int64, Int64) -> C.Decoder result) -> C.Statement () result
statementWithManyRows decoder =
  C.statement template encoder (decoder rowDecoder) True
  where
    template =
      "SELECT generate_series(0,1000) as a, generate_series(1000,2000) as b"
    encoder =
      conquer
    rowDecoder =
      tuple <$> C.column D.int8 <*> C.column D.int8
      where
        tuple !a !b =
          (a, b)

statementWithManyRowsInVector :: C.Statement () (Vector (Int64, Int64))
statementWithManyRowsInVector =
  statementWithManyRows C.rowVector

statementWithManyRowsInRevList :: C.Statement () (List (Int64, Int64))
statementWithManyRowsInRevList =
  statementWithManyRows C.rowRevList

statementWithManyRowsInList :: C.Statement () (List (Int64, Int64))
statementWithManyRowsInList =
  statementWithManyRows C.rowList
