module Main where

import Criterion
import Criterion.Main
import Hasql.Connection qualified as A
import Hasql.Decoders qualified as D
import Hasql.Pipeline qualified as E
import Hasql.Session qualified as B
import Hasql.Statement qualified as C
import Prelude

main :: IO ()
main =
  do
    Right connection <- acquireConnection
    useConnection connection
  where
    acquireConnection =
      A.acquire mempty
    useConnection connection =
      defaultMain
        [ sessionBench "largeResultInVector" sessionWithSingleLargeResultInVector,
          sessionBench "largeResultInList" sessionWithSingleLargeResultInList,
          sessionBench "manyLargeResults" sessionWithManyLargeResults,
          sessionBench "manyLargeResultsViaPipeline" sessionWithManyLargeResultsViaPipeline,
          sessionBench "manySmallResults" sessionWithManySmallResults,
          sessionBench "manySmallResultsViaPipeline" sessionWithManySmallResultsViaPipeline,
          bgroup
            "singleStatementOverhead"
            [ sessionBench "1-sequential" sessionWith1SmallResult,
              sessionBench "1-pipeline" sessionWith1SmallResultViaPipeline,
              sessionBench "10-sequential" sessionWith10SmallResults,
              sessionBench "10-pipeline" sessionWith10SmallResultsViaPipeline,
              sessionBench "100-sequential" sessionWithManySmallResults,
              sessionBench "100-pipeline" sessionWithManySmallResultsViaPipeline
            ]
        ]
      where
        sessionBench :: (NFData a) => String -> B.Session a -> Benchmark
        sessionBench name session =
          bench name (nfIO (A.use connection session >>= either (fail . show) pure))

-- * Sessions

sessionWith1SmallResult :: B.Session (Int32, Int32)
sessionWith1SmallResult =
  B.statement () statementWithSingleRow

sessionWith1SmallResultViaPipeline :: B.Session (Int32, Int32)
sessionWith1SmallResultViaPipeline =
  B.pipeline (E.statement () statementWithSingleRow)

sessionWith10SmallResults :: B.Session [(Int32, Int32)]
sessionWith10SmallResults =
  replicateM 10 (B.statement () statementWithSingleRow)

sessionWith10SmallResultsViaPipeline :: B.Session [(Int32, Int32)]
sessionWith10SmallResultsViaPipeline =
  B.pipeline (replicateM 10 (E.statement () statementWithSingleRow))

sessionWithSingleLargeResultInVector :: B.Session (Vector (Int32, Int32))
sessionWithSingleLargeResultInVector =
  B.statement () statementWithManyRowsInVector

sessionWithSingleLargeResultInList :: B.Session [(Int32, Int32)]
sessionWithSingleLargeResultInList =
  B.statement () statementWithManyRowsInList

sessionWithManyLargeResults :: B.Session [Vector (Int32, Int32)]
sessionWithManyLargeResults =
  replicateM 100 (B.statement () statementWithManyRowsInVector)

sessionWithManySmallResults :: B.Session [(Int32, Int32)]
sessionWithManySmallResults =
  replicateM 100 (B.statement () statementWithSingleRow)

sessionWithManyLargeResultsViaPipeline :: B.Session [Vector (Int32, Int32)]
sessionWithManyLargeResultsViaPipeline =
  B.pipeline (replicateM 100 (E.statement () statementWithManyRowsInVector))

sessionWithManySmallResultsViaPipeline :: B.Session [(Int32, Int32)]
sessionWithManySmallResultsViaPipeline =
  B.pipeline (replicateM 100 (E.statement () statementWithSingleRow))

-- * Statements

statementWithSingleRow :: C.Statement () (Int32, Int32)
statementWithSingleRow =
  C.preparable template encoder decoder
  where
    template =
      "SELECT 1, 2"
    encoder =
      conquer
    decoder =
      D.singleRow row
      where
        row =
          tuple <$> (D.column . D.nonNullable) D.int4 <*> (D.column . D.nonNullable) D.int4
          where
            tuple !a !b =
              (a, b)

statementWithManyRows :: (D.Row (Int32, Int32) -> D.Result result) -> C.Statement () result
statementWithManyRows decoder =
  C.preparable template encoder (decoder rowDecoder)
  where
    template =
      "SELECT generate_series(0,1000) as a, generate_series(1000,2000) as b"
    encoder =
      conquer
    rowDecoder =
      tuple <$> (D.column . D.nonNullable) D.int4 <*> (D.column . D.nonNullable) D.int4
      where
        tuple !a !b =
          (a, b)

statementWithManyRowsInVector :: C.Statement () (Vector (Int32, Int32))
statementWithManyRowsInVector =
  statementWithManyRows D.rowVector

statementWithManyRowsInList :: C.Statement () [(Int32, Int32)]
statementWithManyRowsInList =
  statementWithManyRows D.rowList
