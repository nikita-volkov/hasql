module Hasql.TestingKit.Constants where

import Hasql.Connection qualified as Connection

localConnectionSettings :: Connection.ConnectionString
localConnectionSettings =
  Connection.connectionString host port user password database
  where
    host = "localhost"
    port = 5432
    user = "postgres"
    password = "postgres"
    database = "postgres"
