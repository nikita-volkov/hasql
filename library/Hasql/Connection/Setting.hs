module Hasql.Connection.Setting where

import Hasql.Connection.Setting.Connection
import Hasql.Prelude

data Setting

connectionString :: ConnectionString -> Setting
connectionString =
  error "TODO"

usePreparedStatements :: Bool -> Setting
usePreparedStatements =
  error "TODO"
