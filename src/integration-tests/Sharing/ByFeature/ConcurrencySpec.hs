module Sharing.ByFeature.ConcurrencySpec (spec) where

import Control.Concurrent
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Test.Hspec
import Prelude

spec :: SpecWith (Text, Word16)
spec = do
  it "handles concurrent connections properly" \config -> do
    Scripts.onPreparableConnection config \connection1 -> do
      Scripts.onPreparableConnection config \connection2 -> do
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
          _ <- Connection.use connection1 (Session.statement (0.2 :: Double) selectSleep)
          void (tryPutMVar finishVar False)

        _ <- forkIO do
          takeMVar beginVar
          _ <- Connection.use connection2 (Session.statement (0.1 :: Double) selectSleep)
          void (tryPutMVar finishVar True)

        -- The second connection should finish first (True)
        result <- takeMVar finishVar
        result `shouldBe` True
