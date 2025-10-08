module Helpers.Statements.SelectOne where

import Hasql.Decoders qualified as Decoders
import Hasql.Statement qualified as Statement
import Helpers.Dsls.Statement
import Prelude

data SelectOne = SelectOne

type SelectOneResult = Int32

instance StatementModule SelectOne SelectOneResult where
  statement =
    Statement.Statement
      "select 1"
      mempty
      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
      True
