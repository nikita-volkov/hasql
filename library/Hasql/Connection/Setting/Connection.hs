module Hasql.Connection.Setting.Connection where

import Hasql.Connection.Setting.Connection.Component qualified as Component
import Hasql.Prelude

data ConnectionString

plain :: Text -> ConnectionString
plain =
  error "TODO"

components :: [Component.Component] -> ConnectionString
components =
  error "TODO"
