module Main.Connection
where

import Main.Prelude
import qualified Hasql.Connection as HC
import qualified Hasql.Statement as HQ
import qualified Hasql.Session


with :: (HC.Connection -> IO a) -> IO (Either HC.ConnectionError a)
with handler =
  runExceptT $ acquire >>= \connection -> use connection <* release connection
  where
    acquire =
      ExceptT $ HC.acquire settings
      where
        settings =
          HC.settings host port user password database
          where
            host = "localhost"
            port = 5432
            user = "postgres"
            password = ""
            database = "postgres"
    use connection =
      lift $ handler connection
    release connection =
      lift $ HC.release connection
