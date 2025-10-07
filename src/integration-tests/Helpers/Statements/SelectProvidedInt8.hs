module Helpers.Statements.SelectProvidedInt8 where

import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Statement qualified as Statement
import Prelude

statement :: Statement.Statement Int64 Int64
statement =
  Statement.Statement
    "select $1"
    (Encoders.param (Encoders.nonNullable Encoders.int8))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
    True
