module Hasql.Connection.Setting.Connection
  ( Connection,
    string,
    params,
  )
where

import Hasql.Connection.Config.ConnectionString qualified as Config.ConnectionString
import Hasql.Connection.Config.ConnectionString.Params qualified as Config.ConnectionString.Params
import Hasql.Connection.Setting.Connection.Param qualified as Param
import Hasql.Prelude

newtype Connection = Connection ByteString

instance Config.ConnectionString.Constructs Connection where
  construct = coerce

-- | From plain connection string according to <http://www.postgresql.org/docs/9.4/static/libpq-connect.html#LIBPQ-CONNSTRING the PostgreSQL format>.
string :: Text -> Connection
string =
  Connection . Config.ConnectionString.fromText

-- | From structured parameters.
params :: [Param.Param] -> Connection
params =
  Connection . Config.ConnectionString.fromParams . Config.ConnectionString.Params.fromUpdates
