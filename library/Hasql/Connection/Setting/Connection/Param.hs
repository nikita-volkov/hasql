module Hasql.Connection.Setting.Connection.Param
  ( Param,
    host,
    port,
    user,
    password,
    dbname,
    other,
  )
where

import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Text.Encoding qualified as Text
import Hasql.Connection.Config.ConnectionString.Params qualified as Config
import Hasql.Prelude

newtype Param = Param (Config.Params -> Config.Params)

instance Config.Updates Param where
  update = coerce

host :: Text -> Param
host =
  Param . Config.setKeyValue "host" . Text.encodeUtf8

port :: Word16 -> Param
port =
  Param . Config.setKeyValue "port" . BL.toStrict . BB.toLazyByteString . BB.word16Dec

user :: Text -> Param
user =
  Param . Config.setKeyValue "user" . Text.encodeUtf8

password :: Text -> Param
password =
  Param . Config.setKeyValue "password" . Text.encodeUtf8

dbname :: Text -> Param
dbname =
  Param . Config.setKeyValue "dbname" . Text.encodeUtf8

other :: Text -> Text -> Param
other name =
  Param . Config.setKeyValue (Text.encodeUtf8 name) . Text.encodeUtf8
