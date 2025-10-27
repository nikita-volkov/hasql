module Helpers.Statements.BrokenSyntax where

import Data.Int
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Statement qualified as Statement
import Helpers.Dsls.Statement
import Prelude

data BrokenSyntax = BrokenSyntax
  { start :: Int64,
    end :: Int64
  }

type BrokenSyntaxResult = [Int64]

instance StatementModule BrokenSyntax BrokenSyntaxResult where
  statement =
    Statement.preparable
      "S"
      ( mconcat
          [ start >$< Encoders.param (Encoders.nonNullable Encoders.int8),
            end >$< Encoders.param (Encoders.nonNullable Encoders.int8)
          ]
      )
      ( Decoders.rowList
          (Decoders.column (Decoders.nonNullable Decoders.int8))
      )
