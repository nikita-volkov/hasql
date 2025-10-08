module Helpers.Statements.SelectProvidedInt8 where

import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Statement qualified as Statement
import Helpers.Dsls.Statement
import Prelude

data SelectProvidedInt8 = SelectProvidedInt8
  { value :: Int64
  }

type SelectProvidedInt8Result = Int64

instance StatementModule SelectProvidedInt8 SelectProvidedInt8Result where
  statement =
    Statement.Statement
      "select $1"
      (value >$< Encoders.param (Encoders.nonNullable Encoders.int8))
      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
      True
