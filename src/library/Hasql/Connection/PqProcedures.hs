module Hasql.Connection.PqProcedures where

import Hasql.Connection.PqProcedures.ServerVersion qualified as ServerVersion
import Platform.Prelude
import Pq qualified

{-# INLINE checkConnectionStatus #-}
checkConnectionStatus :: Pq.Connection -> IO (Maybe (Maybe ByteString))
checkConnectionStatus c = do
  s <- Pq.status c
  case s of
    Pq.ConnectionOk -> return Nothing
    _ -> fmap Just (Pq.errorMessage c)

{-# INLINE checkServerVersion #-}
checkServerVersion :: Pq.Connection -> IO (Maybe Text)
checkServerVersion c = do
  versionInt <- Pq.serverVersion c
  pure
    let version = ServerVersion.fromInt versionInt
        minVersion = ServerVersion.ServerVersion 10 0 0
     in if version < minVersion
          then
            Just ("Server version is lower than 10: " <> ServerVersion.toText version)
          else
            Nothing

{-# INLINE initConnection #-}
initConnection :: Pq.Connection -> IO ()
initConnection c =
  void (Pq.exec c sql)
  where
    sql =
      "SET client_encoding = 'UTF8';\n\
      \SET client_min_messages TO WARNING;"
