module Main where

import Hasql.Connection qualified
import Hasql.Session qualified
import Hasql.TestingKit.TestcontainersHelpers qualified as TestcontainersHelpers
import Main.Statements qualified as Statements
import TestcontainersPostgresql qualified
import Prelude

main :: IO ()
main =
  TestcontainersPostgresql.run TestcontainersHelpers.defaultTestcontainerConfig $ \(host, port) -> do
    let connectionSettings = TestcontainersHelpers.connectionSettingsFromHostPort host port
    acquire connectionSettings >>= use
  where
    acquire connectionSettings =
      (,) <$> acquire connectionSettings <*> acquire connectionSettings
      where
        acquire cs =
          join
            $ fmap (either (fail . show) return)
            $ Hasql.Connection.acquire cs
    use (connection1, connection2) =
      do
        beginVar <- newEmptyMVar
        finishVar <- newEmptyMVar
        forkIO $ do
          traceM "1: in"
          putMVar beginVar ()
          session connection1 (Hasql.Session.statement 0.2 Statements.selectSleep)
          traceM "1: out"
          void (tryPutMVar finishVar False)
        forkIO $ do
          takeMVar beginVar
          traceM "2: in"
          session connection2 (Hasql.Session.statement 0.1 Statements.selectSleep)
          traceM "2: out"
          void (tryPutMVar finishVar True)
        bool exitFailure exitSuccess . traceShowId =<< takeMVar finishVar
      where
        session connection session =
          Hasql.Session.run session connection
            >>= either (fail . show) return
