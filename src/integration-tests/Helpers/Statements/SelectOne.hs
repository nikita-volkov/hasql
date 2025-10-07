module Helpers.Statements.SelectOne where

import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Prelude

statement =
  Statement.Statement
    "select 1"
    mempty
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
    True
