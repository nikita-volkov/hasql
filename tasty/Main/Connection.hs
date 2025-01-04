module Main.Connection where

import Hasql.Connection qualified as HC
import Main.Prelude

with :: (HC.Connection -> IO a) -> IO (Either HC.ConnectionError a)
with handler =
  runExceptT $ acquire >>= \connection -> use connection <* release connection
  where
    acquire =
      ExceptT $ HC.acquire True settings
      where
        settings =
          HC.settings host port user password database
          where
            host = "localhost"
            port = 5432
            user = "postgres"
            password = "postgres"
            database = "postgres"
    use connection =
      lift $ handler connection
    release connection =
      lift $ HC.release connection
