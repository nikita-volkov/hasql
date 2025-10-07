module Helpers.NewStatements.SetConfig where

import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Statement qualified as Statement
import Helpers.Frameworks.Statement
import Prelude hiding (local)

data SetConfig = SetConfig
  { name :: Text,
    value :: Text,
    local :: Bool
  }

type SetConfigResult = ()

instance StatementModule SetConfig SetConfigResult where
  statement =
    Statement.Statement sql encoder decoder preparable
    where
      sql =
        "SELECT set_config($1, $2, $3)"

      encoder =
        mconcat
          [ name >$< Encoders.param (Encoders.nonNullable Encoders.text),
            value >$< Encoders.param (Encoders.nonNullable Encoders.text),
            local >$< Encoders.param (Encoders.nonNullable Encoders.bool)
          ]

      decoder =
        Decoders.noResult

      preparable =
        True
