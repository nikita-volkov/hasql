module Hasql.Core.ParseDataRow where

import Hasql.Prelude
import qualified Hasql.Core.ParseDataRowColumn as A
import qualified BinaryParser as D
import qualified VectorBuilder.Builder as N
import qualified VectorBuilder.Vector as O


{-|
A specification for processing of DataRow and RowDescription messages.
-}
data ParseDataRow result =
  {-|
  * Amount of columns
  * Builder of column OID vector (for composition)
  * Thunk of the materialized OID vector (for reuse)
  * Parser with integer datetimes on
  * Parser with integer datetimes off
  -}
  ParseDataRow !Int (N.Builder Word32) (Vector Word32) (D.BinaryParser result) (D.BinaryParser result)

deriving instance Functor ParseDataRow

instance Applicative ParseDataRow where
  pure x =
    ParseDataRow 0 mempty empty (pure x) (pure x)
  (<*>) (ParseDataRow leftCols leftOidBuilder leftOidVec leftParser1 leftParser2)
        (ParseDataRow rightCols rightOidBuilder rightOidVec rightParser1 rightParser2) =
    ParseDataRow
      (leftCols + rightCols)
      (oidBuilder)
      (O.build oidBuilder)
      (leftParser1 <*> rightParser1)
      (leftParser2 <*> rightParser2)
    where
      oidBuilder =
        leftOidBuilder <> rightOidBuilder

column :: A.ParseDataRowColumn column -> ParseDataRow column
column (A.ParseDataRowColumn oid parser1 parser2) =
  ParseDataRow 1 (N.singleton oid) (pure oid) parser1 parser2
