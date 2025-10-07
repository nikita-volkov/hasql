module Helpers.Statements.SelectOne where

import Hasql.Decoders qualified as Decoders
import Hasql.Statement qualified as Statement
import Prelude

statement :: Statement.Statement () Int32
statement =
  Statement.Statement
    "select 1"
    mempty
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
    True
