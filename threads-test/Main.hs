module Main where

import Hasql.Connection qualified
import Hasql.Session qualified
import Main.Statements qualified as Statements
import Prelude

main :: IO ()
main =
  acquire >>= use
  where
    acquire =
      (,) <$> acquire <*> acquire
      where
        acquire =
          join
            $ fmap (either (fail . show) return)
            $ Hasql.Connection.acquire True connectionSettings
          where
            connectionSettings =
              Hasql.Connection.connectionString "localhost" 5432 "postgres" "postgres" "postgres"
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
