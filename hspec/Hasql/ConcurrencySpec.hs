module Hasql.ConcurrencySpec (spec) where

import Control.Concurrent
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Hasql.TestingKit.Testcontainers qualified as Testcontainers
import Test.Hspec
import Prelude

spec :: Spec
spec = aroundAll Testcontainers.withConnection do
  describe "Concurrency" do
    it "handles concurrent connections properly" \_ -> do
      -- We need two separate connections for this test
      Testcontainers.withConnectionSettings \settings -> do
        connection1 <- Connection.acquire settings >>= either (fail . show) return
        connection2 <- Connection.acquire settings >>= either (fail . show) return

        let selectSleep =
              Statement.Statement
                "select pg_sleep($1)"
                (Encoders.param (Encoders.nonNullable Encoders.float8))
                Decoders.noResult
                True

        beginVar <- newEmptyMVar
        finishVar <- newEmptyMVar

        _ <- forkIO do
          putMVar beginVar ()
          _ <- Session.run (Session.statement (0.2 :: Double) selectSleep) connection1
          void (tryPutMVar finishVar False)

        _ <- forkIO do
          takeMVar beginVar
          _ <- Session.run (Session.statement (0.1 :: Double) selectSleep) connection2
          void (tryPutMVar finishVar True)

        -- The second connection should finish first (True)
        result <- takeMVar finishVar
        result `shouldBe` True
