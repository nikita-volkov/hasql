module Hasql.TestingUtils.Constants where

import Hasql.Connection qualified as Connection

localConnectionSettings :: Connection.Settings
localConnectionSettings =
  Connection.settings host port user password database
  where
    host = "localhost"
    port = 5432
    user = "postgres"
    password = "postgres"
    database = "postgres"
