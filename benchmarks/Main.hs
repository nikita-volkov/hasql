module Main where

import Prelude
import Bug
import Criterion
import Criterion.Main
import qualified Hasql.Connection as A
import qualified Hasql.Batch as J
import qualified Hasql.Session as F
import qualified Hasql.Statement as G
import qualified Hasql.DecodeResult as B
import qualified Hasql.DecodeRow as C
import qualified Hasql.DecodePrimitive as D
import qualified Data.Vector as H
import qualified Control.Foldl as I


main =
  do
    connection <- connect
    let
      sessionBench :: NFData a => String -> F.Session a -> Benchmark
      sessionBench name session =
        bench name (nfIO (runSession session))
        where
          runSession session =
            do
              Right result <- A.session connection session
              return result
      in
        defaultMain
        [
          sessionBench "singleLargeResultInVector" singleLargeResultInVectorSession,
          sessionBench "singleLargeResultInRevList" singleLargeResultInRevListSession,
          sessionBench "manyLargeResultsInVector" manyLargeResultsInVectorSession,
          sessionBench "manyLargeResultsInVectorInBatch" manyLargeResultsInVectorInBatchSession,
          sessionBench "manySmallResults" manySmallResultsSession,
          sessionBench "manySmallResultsInBatch" manySmallResultsInBatchSession
        ]

connect :: IO A.Connection
connect =
  do
    openingResult <- A.open (A.TCPConnectionSettings "localhost" 5432) "postgres" "" Nothing handleErrorOrNotification
    case openingResult of
      Left error -> fail (showString "Can't connect: " (show error))
      Right connection -> return connection
  where
    handleErrorOrNotification x =
      putStrLn ("Async event: " <> show x)

-- * Sessions
-------------------------

singleLargeResultInVectorSession :: F.Session (Vector (Int64, Int64))
singleLargeResultInVectorSession =
  F.batch (manyRowsBatch B.vector)

singleLargeResultInRevListSession :: F.Session [(Int64, Int64)]
singleLargeResultInRevListSession =
  F.batch (manyRowsBatch B.revList)

manyLargeResultsInVectorSession :: F.Session [Vector (Int64, Int64)]
manyLargeResultsInVectorSession =
  replicateM 1000 (F.batch (manyRowsBatch B.vector))

manyLargeResultsInVectorInBatchSession :: F.Session [Vector (Int64, Int64)]
manyLargeResultsInVectorInBatchSession =
  F.batch (replicateM 1000 (manyRowsBatch B.vector))

manySmallResultsSession :: F.Session [(Int64, Int64)]
manySmallResultsSession =
  replicateM 1000 (F.batch singleRowBatch)

manySmallResultsInBatchSession :: F.Session [(Int64, Int64)]
manySmallResultsInBatchSession =
  F.batch (replicateM 1000 singleRowBatch)

-- * Queries
-------------------------

singleRowBatch :: J.Batch (Int64, Int64)
singleRowBatch =
  J.statement (G.prepared "select 1, 2" conquer decode) ()
  where
    decode =
      B.head ((,) <$> C.primitive D.int8 <*> C.primitive D.int8)

{-# INLINE manyRowsBatch #-}
manyRowsBatch :: (C.DecodeRow (Int64, Int64) -> B.DecodeResult result) -> J.Batch result
manyRowsBatch decodeResult =
  J.statement (G.prepared template mempty decode) ()
  where
    template =
      "SELECT generate_series(0,1000) as a, generate_series(1000,2000) as b"
    decode =
      decodeResult $
      tuple <$> C.primitive D.int8 <*> C.primitive D.int8
        where
        tuple !a !b =
          (a, b)
