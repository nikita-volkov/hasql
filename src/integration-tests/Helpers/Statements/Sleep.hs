module Helpers.Statements.Sleep where

import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Statement qualified as Statement
import Prelude

statement :: Statement.Statement Double ()
statement =
  Statement.Statement
    "select pg_sleep($1)"
    (Encoders.param (Encoders.nonNullable Encoders.float8))
    Decoders.noResult
    True
