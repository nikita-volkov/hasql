module Main where

import Prelude
import Bug
import Criterion
import Criterion.Main
import qualified Hasql.Connection as A
import qualified Hasql.Query as J
import qualified Hasql.Interact as F
import qualified Hasql.Model as E
import qualified Hasql.DecodeResult as B
import qualified Hasql.DecodeRow as C
import qualified Hasql.DecodePrimitive as D
import qualified Data.Vector as H
import qualified Control.Foldl as I


main =
  do
    connection <- connect
    let
      interactBench :: NFData a => String -> F.Interact a -> Benchmark
      interactBench name interact =
        bench name (nfIO (runInteract interact))
        where
          runInteract interact =
            do
              Right result <- A.interact connection interact
              return result
      in
        defaultMain
        [
          interactBench "singleLargeResultInVector" singleLargeResultInVectorInteract,
          interactBench "singleLargeResultInRevList" singleLargeResultInRevListInteract,
          interactBench "manyLargeResultsInVector" manyLargeResultsInVectorInteract,
          interactBench "manyLargeResultsInVectorInBatch" manyLargeResultsInVectorInBatchInteract,
          interactBench "manySmallResults" manySmallResultsInteract,
          interactBench "manySmallResultsInBatch" manySmallResultsInBatchInteract
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

-- * Interactions
-------------------------

singleLargeResultInVectorInteract :: F.Interact (Vector (Int64, Int64))
singleLargeResultInVectorInteract =
  F.query (manyRowsQuery (B.rows I.vector))

singleLargeResultInRevListInteract :: F.Interact [(Int64, Int64)]
singleLargeResultInRevListInteract =
  F.query (manyRowsQuery (B.rows I.revList))

manyLargeResultsInVectorInteract :: F.Interact [Vector (Int64, Int64)]
manyLargeResultsInVectorInteract =
  replicateM 1000 (F.query (manyRowsQuery (B.rows I.vector)))

manyLargeResultsInVectorInBatchInteract :: F.Interact [Vector (Int64, Int64)]
manyLargeResultsInVectorInBatchInteract =
  F.query (replicateM 1000 (manyRowsQuery (B.rows I.vector)))

manySmallResultsInteract :: F.Interact [(Int64, Int64)]
manySmallResultsInteract =
  replicateM 1000 (F.query singleRowQuery)

manySmallResultsInBatchInteract :: F.Interact [(Int64, Int64)]
manySmallResultsInBatchInteract =
  F.query (replicateM 1000 singleRowQuery)

-- * Queries
-------------------------

singleRowQuery :: J.Query (Int64, Int64)
singleRowQuery =
  J.preparedStatement "select 1, 2" mempty decode
  where
    decode =
      B.row ((,) <$> C.nonNullPrimitive D.int8 <*> C.nonNullPrimitive D.int8)

{-# INLINE manyRowsQuery #-}
manyRowsQuery :: (C.DecodeRow (Int64, Int64) -> B.DecodeResult result) -> J.Query result
manyRowsQuery decodeResult =
  J.preparedStatement template mempty decode
  where
    template =
      "SELECT generate_series(0,1000) as a, generate_series(1000,2000) as b"
    decode =
      decodeResult $
      tuple <$> C.nonNullPrimitive D.int8 <*> C.nonNullPrimitive D.int8
        where
        tuple !a !b =
          (a, b)
