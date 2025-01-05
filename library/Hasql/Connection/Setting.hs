module Hasql.Connection.Setting
  ( Setting,
    connectionString,
    usePreparedStatements,
  )
where

import Hasql.Connection.Config qualified as Config
import Hasql.Connection.Setting.Connection
import Hasql.Prelude

newtype Setting = Setting (Config.Config -> Config.Config)

instance Config.Updates Setting where
  update (Setting update) = update

connectionString :: ConnectionString -> Setting
connectionString =
  error "TODO"

usePreparedStatements :: Bool -> Setting
usePreparedStatements =
  error "TODO"
