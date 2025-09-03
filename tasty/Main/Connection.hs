module Main.Connection where

import Hasql.Connection qualified as HC
import Hasql.TestingKit.Constants qualified as Constants
import Main.Prelude

with :: (HC.Connection -> IO a) -> IO (Either HC.ConnectionError a)
with handler =
  runExceptT $ acquire >>= \connection -> use connection <* release connection
  where
    acquire =
      ExceptT $ HC.acquire Constants.localConnectionSettings
    use connection =
      lift $ handler connection
    release connection =
      lift $ HC.release connection

withEmptyPassword :: (HC.Connection -> IO a) -> IO (Either HC.ConnectionError a)
withEmptyPassword handler =
  runExceptT $ acquire >>= \connection -> use connection <* release connection
  where
    acquire =
      ExceptT $ HC.acquire Constants.localConnectionSettingsEmptyPassword
    use connection =
      lift $ handler connection
    release connection =
      lift $ HC.release connection
