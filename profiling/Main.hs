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
    Right !result <- fmap force <$> A.session connection (session 10 200 100)
    Right !result <- fmap force <$> A.session connection (session 200 10 100)
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

session :: Int -> Int -> Int -> F.Session [[[(Int64, Int64)]]]
session amountOfQueries amountOfStatements amountOfRows =
  replicateM amountOfQueries (F.batch (replicateM amountOfStatements (manyRowsBatch amountOfRows (B.revList))))
  where
    replicateM cnt0 f =
      loop cnt0
      where
        loop cnt
            | cnt <= 0  = pure []
            | otherwise = liftA2 (:) f (loop (cnt - 1))

-- * Queries
-------------------------

manyRowsBatch :: Int -> (C.DecodeRow (Int64, Int64) -> B.DecodeResult result) -> J.Batch result
manyRowsBatch amountOfRows decodeResult =
  J.statement (G.unprepared template conquer decode) ()
  where
    template =
      "SELECT generate_series(0," <> fromString (show amountOfRows) <> ") as a, generate_series(10000," <> fromString (show (amountOfRows + 10000)) <> ") as b"
    decode =
      decodeResult $
      tuple <$> C.primitive D.int8 <*> C.primitive D.int8
        where
        tuple !a !b =
          (a, b)
