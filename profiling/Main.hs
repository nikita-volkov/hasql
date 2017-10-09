module Main where

import Prelude hiding (interact)
import Bug
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
    Right !result <- fmap force <$> A.interact connection (interact 10 100 100)
    Right !result <- fmap force <$> A.interact connection (interact 10 100 100)
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

-- * Interactions
-------------------------

interact :: Int -> Int -> Int -> F.Interact [[[(Int64, Int64)]]]
interact amountOfQueries amountOfStatements amountOfRows =
  replicateM amountOfQueries (F.query (replicateM amountOfStatements (manyRowsQuery amountOfRows (B.rows I.revList))))
  where
    replicateM cnt0 f =
      loop cnt0
      where
        loop cnt
            | cnt <= 0  = pure []
            | otherwise = liftA2 (:) f (loop (cnt - 1))

-- * Queries
-------------------------

manyRowsQuery :: Int -> (C.DecodeRow (Int64, Int64) -> B.DecodeResult result) -> J.Query result
manyRowsQuery amountOfRows decodeResult =
  J.preparedStatement template mempty decode
  where
    template =
      "SELECT generate_series(0," <> fromString (show amountOfRows) <> ") as a, generate_series(10000," <> fromString (show (amountOfRows + 10000)) <> ") as b"
    decode =
      decodeResult $
      tuple <$> C.nonNullPrimitive D.int8 <*> C.nonNullPrimitive D.int8
        where
        tuple !a !b =
          (a, b)
