module Helpers.Statements.Sleep where

import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Statement qualified as Statement
import Helpers.Dsls.Statement
import Prelude hiding (local)

data Sleep = Sleep
  { seconds :: Double
  }

type SleepResult = ()

instance StatementModule Sleep SleepResult where
  statement =
    Statement.Statement
      "select pg_sleep($1)"
      (seconds >$< Encoders.param (Encoders.nonNullable Encoders.float8))
      Decoders.noResult
      True
