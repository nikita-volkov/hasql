module Helpers.Statements.CountPreparedStatements where

import Hasql.Decoders qualified as Decoders
import Hasql.Statement qualified as Statement
import Helpers.Dsls.Statement
import Prelude

data CountPreparedStatements = CountPreparedStatements

type CountPreparedStatementsResult = Int32

instance StatementModule CountPreparedStatements CountPreparedStatementsResult where
  statement =
    Statement.Statement
      "select count(*)::int4 from pg_prepared_statements"
      mempty
      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
      False
