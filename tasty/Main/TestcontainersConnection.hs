module Main.TestcontainersConnection where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Hasql.Connection qualified as HC
import Hasql.TestingKit.TestcontainersHelpers qualified as TestcontainersHelpers
import TestcontainersPostgresql qualified
import Prelude

with :: (HC.Connection -> IO a) -> IO (Either HC.ConnectionError a)
with handler = do
  resultRef <- newIORef (Left Nothing) -- dummy initial value
  TestcontainersPostgresql.run TestcontainersHelpers.defaultTestcontainerConfig $ \(host, port) -> do
    let connectionSettings = TestcontainersHelpers.connectionSettingsFromHostPort host port
    result <- runExceptT $ do
      connection <- ExceptT $ HC.acquire connectionSettings
      result <- lift $ handler connection
      lift $ HC.release connection
      pure result
    writeIORef resultRef result
  readIORef resultRef
