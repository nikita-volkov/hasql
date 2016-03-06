module Main where

import Rebase.Prelude
import qualified Hasql.Connection
import qualified Hasql.Query
import qualified Hasql.Encoders
import qualified Hasql.Decoders
import qualified Hasql.Session
import qualified Main.Queries as Queries


main =
  acquire >>= use
  where
    acquire =
      (,) <$> acquire <*> acquire
      where
        acquire =
          join $
          fmap (either (fail . show) return) $
          Hasql.Connection.acquire connectionSettings
          where
            connectionSettings =
              Hasql.Connection.settings "localhost" 5432 "postgres" "" "postgres"
    use (connection1, connection2) =
      do
        beginVar <- newEmptyMVar
        finishVar <- newEmptyMVar
        forkIO $ do
          traceM "1: in"
          putMVar beginVar ()
          session connection1 (Hasql.Session.query 0.2 Queries.selectSleep)
          traceM "1: out"
          void (tryPutMVar finishVar False)
        forkIO $ do
          takeMVar beginVar
          traceM "2: in"
          session connection2 (Hasql.Session.query 0.1 Queries.selectSleep)
          traceM "2: out"
          void (tryPutMVar finishVar True)
        bool exitFailure exitSuccess . traceShowId =<< takeMVar finishVar
      where
        session connection session =
          Hasql.Session.run session connection >>=
          either (fail . show) return
