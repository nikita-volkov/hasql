module Hasql.Connection.PqProcedures where

import Hasql.Connection.ServerVersion qualified as ServerVersion
import Platform.Prelude
import Pq qualified

{-# INLINE initConnection #-}
initConnection :: Pq.Connection -> IO ()
initConnection c =
  void (Pq.exec c sql)
  where
    sql =
      "SET client_encoding = 'UTF8';\n\
      \SET client_min_messages TO WARNING;"
