module Helpers.Statements.CurrentSetting where

import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Statement qualified as Statement
import Helpers.Frameworks.Statement
import Prelude

data CurrentSetting = CurrentSetting
  { name :: Text,
    missingOk :: Bool
  }

type CurrentSettingResult = Maybe Text

instance StatementModule CurrentSetting CurrentSettingResult where
  statement =
    Statement.Statement sql encoder decoder preparable
    where
      sql =
        "SELECT current_setting($1, $2)"

      encoder =
        mconcat
          [ name >$< Encoders.param (Encoders.nonNullable Encoders.text),
            missingOk >$< Encoders.param (Encoders.nonNullable Encoders.bool)
          ]

      decoder =
        Decoders.singleRow
          (Decoders.column (Decoders.nullable Decoders.text))

      preparable =
        True
