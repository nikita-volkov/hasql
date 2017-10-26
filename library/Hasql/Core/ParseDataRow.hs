module Hasql.Core.ParseDataRow where

import Hasql.Prelude
import qualified Ptr.Parse as B
import qualified Hasql.Core.Protocol.Parse.Primitives as C


{-|
A specification for processing of DataRow messages.

It is assumed that the size of the input vector is checked externally.
-}
data ParseDataRow result =
  ParseDataRow {-# UNPACK #-} !Int (B.Parse result)

deriving instance Functor ParseDataRow

instance Applicative ParseDataRow where
  pure x =
    ParseDataRow 0 (pure x)
  (<*>) (ParseDataRow leftSize leftParse) (ParseDataRow rightSize rightParse) =
    ParseDataRow (leftSize + rightSize) (leftParse <*> rightParse)

nullableColumn :: B.Parse column -> ParseDataRow (Maybe column)
nullableColumn parseColumn =
  ParseDataRow 1 (C.nullableDataRowColumn parseColumn)

column :: B.Parse column -> ParseDataRow column
column parseColumn =
  ParseDataRow 1 (C.nonNullDataRowColumn parseColumn)
