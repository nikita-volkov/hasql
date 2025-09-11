module Main where

import Data.Vector qualified as F
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as D
import Hasql.Session qualified as B
import Hasql.Statement qualified as C
import Hasql.TestingKit.Testcontainers qualified as Testcontainers
import Prelude

main :: IO ()
main =
  Testcontainers.withConnection \connection -> do
    traceEventIO "START Session"
    Right _ <- Connection.use connection sessionWithManySmallResults
    traceEventIO "STOP Session"
    return ()

-- * Sessions

sessionWithManySmallParameters :: Vector (Int64, Int64) -> B.Session ()
sessionWithManySmallParameters =
  error "TODO: sessionWithManySmallParameters"

sessionWithSingleLargeResultInVector :: B.Session (Vector (Int64, Int64))
sessionWithSingleLargeResultInVector =
  B.statement () statementWithManyRowsInVector

sessionWithSingleLargeResultInList :: B.Session [(Int64, Int64)]
sessionWithSingleLargeResultInList =
  B.statement () statementWithManyRowsInList

sessionWithManySmallResults :: B.Session (Vector (Int64, Int64))
sessionWithManySmallResults =
  F.replicateM 1000 (B.statement () statementWithSingleRow)

-- * Statements

statementWithManyParameters :: C.Statement (Vector (Int64, Int64)) ()
statementWithManyParameters =
  error "TODO: statementWithManyParameters"

statementWithSingleRow :: C.Statement () (Int64, Int64)
statementWithSingleRow =
  C.Statement template encoder decoder True
  where
    template =
      "SELECT 1, 2"
    encoder =
      conquer
    decoder =
      D.singleRow row
      where
        row =
          tuple <$> (D.column . D.nonNullable) D.int8 <*> (D.column . D.nonNullable) D.int8
          where
            tuple !a !b =
              (a, b)

statementWithManyRows :: (D.Row (Int64, Int64) -> D.Result result) -> C.Statement () result
statementWithManyRows decoder =
  C.Statement template encoder (decoder rowDecoder) True
  where
    template =
      "SELECT generate_series(0,1000) as a, generate_series(1000,2000) as b"
    encoder =
      conquer
    rowDecoder =
      tuple <$> (D.column . D.nonNullable) D.int8 <*> (D.column . D.nonNullable) D.int8
      where
        tuple !a !b =
          (a, b)

statementWithManyRowsInVector :: C.Statement () (Vector (Int64, Int64))
statementWithManyRowsInVector =
  statementWithManyRows D.rowVector

statementWithManyRowsInList :: C.Statement () [(Int64, Int64)]
statementWithManyRowsInList =
  statementWithManyRows D.rowList
