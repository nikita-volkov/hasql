module Hasql.Core.ParseDataRowColumn where

import Hasql.Prelude
import qualified BinaryParser as D


{-|
Parser of a single column in the DataRow message.
-}
data ParseDataRowColumn result =
  {-|
  * OID
  * Parser with integer datetimes on
  * Parser with integer datetimes off
  -}
  ParseDataRowColumn Word32 (D.BinaryParser result) (D.BinaryParser result)

deriving instance Functor ParseDataRowColumn
