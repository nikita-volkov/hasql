module Main where

import Prelude hiding (session)
import Bug
import qualified Hasql.Connection as A
import qualified Hasql.Batch as J
import qualified Hasql.Session as F
import qualified Hasql.DecodeResult as B
import qualified Hasql.DecodeRow as C
import qualified Hasql.DecodePrimitive as D
import qualified Hasql.Statement as G
import qualified Data.Vector as H
import qualified Control.Foldl as I


main =
  do
    connection <- connect
    traceEventIO "START Session"
    Right !result <- A.session connection (batchSession 10 10 (nonDecodingBatch 10000))
    traceEventIO "STOP Session"
    return ()

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

{-# INLINE manyRowsSession #-}
manyRowsSession :: Int -> Int -> Int -> F.Session [[[(Int32, Int32)]]]
manyRowsSession amountOfQueries amountOfStatements amountOfRows =
  batchSession amountOfQueries amountOfStatements (manyRowsBatch amountOfRows B.revList)

{-# INLINE batchSession #-}
batchSession :: Int -> Int -> J.Batch batchResult -> F.Session [[batchResult]]
batchSession amountOfQueries amountOfStatements batch =
  replicateM amountOfQueries (F.batch (replicateM amountOfStatements batch))
  where
    replicateM cnt0 f =
      loop cnt0
      where
        loop cnt
            | cnt <= 0  = pure []
            | otherwise = liftA2 (:) f (loop (cnt - 1))

-- * Queries
-------------------------

manyRowsBatch :: Int -> (C.DecodeRow (Int32, Int32) -> B.DecodeResult result) -> J.Batch result
manyRowsBatch amountOfRows decodeResult =
  J.statement (G.unprepared template conquer decode) ()
  where
    template =
      "SELECT generate_series(0," <> fromString (show amountOfRows) <> ") as a, generate_series(10000," <> fromString (show (amountOfRows + 10000)) <> ") as b"
    decode =
      decodeResult $
      tuple <$> C.primitive D.int4 <*> C.primitive D.int4
        where
        tuple !a !b =
          (a, b)

singleColumnRowBatch :: Int -> (C.DecodeRow Int32 -> B.DecodeResult result) -> J.Batch result
singleColumnRowBatch amountOfRows decodeResult =
  J.statement (G.unprepared template conquer decode) ()
  where
    template =
      "SELECT generate_series(0," <> fromString (show amountOfRows) <> ") as a"
    decode =
      decodeResult (C.primitive D.int4)

nonDecodingBatch :: Int -> J.Batch ()
nonDecodingBatch amountOfRows =
  J.statement (G.unprepared template conquer B.ignore) ()
  where
    template =
      "SELECT generate_series(0," <> fromString (show amountOfRows) <> ") as a"
