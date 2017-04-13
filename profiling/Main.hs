module Main where

import Prelude
import Bug
import qualified Hasql.Connection as A
import qualified Hasql.Connection.Session as B
import qualified Hasql.Connection.Session.Statement as C
import qualified Hasql.Connection.Session.Statement.Decoding as D
import qualified Hasql.Connection.Session.Statement.Encoding as E
import qualified Data.Vector as F


main =
  do
    Right connection <- A.acquire "localhost" Nothing "postgres" Nothing Nothing
    session <- parseArgs
    traceEventIO "START Session"
    Right result <- A.use connection session
    traceEventIO "STOP Session"
    return ()
  where
    parseArgs =
      do
        args <- getArgs
        case args of
          "interesting" : _ -> return interestingSession
          "simple" : _ -> return simpleSession
          _ -> fail "Unknown session"


-- * Sessions
-------------------------

sessionWithSingleLargeResultInList :: B.Session ()
sessionWithSingleLargeResultInList =
  B.batch (B.statement statementWithManyRowsInList ()) $> ()

interestingSession :: B.Session ()
interestingSession =
  replicateM 3 (B.batch (replicateM 3 (B.statement (statementWithAverageRows C.rowList) ()))) $> ()

simpleSession :: B.Session ()
simpleSession =
  B.batch (B.statement (statementWithAverageRows C.rowList) ()) $> ()


-- * Statements
-------------------------

statementWithSingleRow :: C.Statement () (Int64, Int64)
statementWithSingleRow =
  C.statement template encoder decoder True
  where
    template =
      "SELECT 1, 2"
    encoder =
      conquer
    decoder =
      {-# SCC "statementWithSingleRow/decoder" #-} 
      C.row row
      where
        row =
          tuple <$> C.column D.int8 <*> C.column D.int8
          where
            tuple !a !b =
              (a, b)

statementWithManyRows :: (C.RowDecoder (Int64, Int64) -> C.Decoder result) -> C.Statement () result
statementWithManyRows decoder =
  C.statement template encoder ({-# SCC "statementWithManyRows/decoder" #-} decoder rowDecoder) True
  where
    template =
      "SELECT generate_series(0,1000) as a, generate_series(1000,2000) as b"
    encoder =
      conquer
    rowDecoder =
      {-# SCC "statementWithManyRows/rowDecoder" #-} 
      tuple <$> C.column D.int8 <*> C.column D.int8
      where
        tuple !a !b =
          (a, b)

statementWithAverageRows :: (C.RowDecoder (Int64, Int64) -> C.Decoder result) -> C.Statement () result
statementWithAverageRows decoder =
  C.statement template encoder ({-# SCC "statementWithAverageRows/decoder" #-} decoder rowDecoder) True
  where
    template =
      "SELECT generate_series(0,100) as a, generate_series(100,200) as b"
    encoder =
      conquer
    rowDecoder =
      {-# SCC "statementWithAverageRows/rowDecoder" #-} 
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
