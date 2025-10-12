module Helpers.Statements.GenerateSeries where

import Data.Int
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Statement qualified as Statement
import Helpers.Dsls.Statement
import Prelude

data GenerateSeries = GenerateSeries
  { start :: Int64,
    end :: Int64
  }

type GenerateSeriesResult = [Int64]

instance StatementModule GenerateSeries GenerateSeriesResult where
  statement =
    Statement.Statement
      "SELECT generate_series($1, $2)"
      ( (contramap start (Encoders.param (Encoders.nonNullable Encoders.int8)))
          <> (contramap end (Encoders.param (Encoders.nonNullable Encoders.int8)))
      )
      ( Decoders.rowList
          (Decoders.column (Decoders.nonNullable Decoders.int8))
      )
      True
