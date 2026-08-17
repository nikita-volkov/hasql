module Helpers.Statements.SelectPreparedStatementNames where

import Hasql.Decoders qualified as Decoders
import Hasql.Statement qualified as Statement
import Helpers.Dsls.Statement
import Prelude

data SelectPreparedStatementNames = SelectPreparedStatementNames

type SelectPreparedStatementNamesResult = [Text]

instance StatementModule SelectPreparedStatementNames SelectPreparedStatementNamesResult where
  statement =
    Statement.unpreparable
      "select name from pg_prepared_statements order by name"
      mempty
      (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.text)))
