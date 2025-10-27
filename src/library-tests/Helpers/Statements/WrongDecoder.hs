module Helpers.Statements.WrongDecoder where

import Data.Int
import Data.UUID qualified
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Statement qualified as Statement
import Helpers.Dsls.Statement
import Prelude

data WrongDecoder = WrongDecoder
  { start :: Int64,
    end :: Int64
  }

type WrongDecoderResult = [Data.UUID.UUID]

instance StatementModule WrongDecoder WrongDecoderResult where
  statement =
    Statement.preparable
      "SELECT generate_series($1, $2)"
      ( mconcat
          [ start >$< Encoders.param (Encoders.nonNullable Encoders.int8),
            end >$< Encoders.param (Encoders.nonNullable Encoders.int8)
          ]
      )
      ( Decoders.rowList
          (Decoders.column (Decoders.nonNullable Decoders.uuid))
      )
