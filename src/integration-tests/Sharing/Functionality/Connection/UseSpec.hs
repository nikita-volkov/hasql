module Sharing.Functionality.Connection.UseSpec (spec) where

import Data.Either
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Helpers.Statements.SelectOne qualified as Statements.SelectOne
import Helpers.Statements.Sleep qualified as Statements.Sleep
import Test.Hspec
import Prelude

spec :: SpecWith (Text, Word16)
spec = do
  describe "Transactions" do
    it "Do not cause \"in progress after error\"" \config -> do
      Scripts.onConnection config \connection -> do
        let sumStatement =
              Statement.Statement
                "select ($1 + $2)"
                ( mconcat
                    [ fst >$< Encoders.param (Encoders.nonNullable Encoders.int8),
                      snd >$< Encoders.param (Encoders.nonNullable Encoders.int8)
                    ]
                )
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
                True

        result <-
          Connection.use connection do
            Session.script "."

        result `shouldSatisfy` isLeft

        result <-
          Connection.use connection do
            Session.script "begin;"
            s <- Session.statement (1 :: Int64, 2 :: Int64) sumStatement
            Session.script "end;"
            return s

        result `shouldBe` Right (3 :: Int64)

  describe "Timing out" do
    describe "On a statement" do
      it "Leaves the connection usable" \config -> Scripts.onConnection config \connection -> do
        result <-
          timeout 50_000 do
            Connection.use connection do
              Session.statement (0.1 :: Double) Statements.Sleep.statement

        result `shouldBe` Nothing

        result <-
          Connection.use connection do
            Session.statement () Statements.SelectOne.statement

        result `shouldBe` Right 1

    describe "On a transaction" do
      it "Leaves the connection usable" \config -> Scripts.onConnection config \connection -> do
        -- Start a transaction and timeout during it
        result <-
          timeout 50_000 do
            Connection.use connection do
              Session.script "begin;"
              Session.statement (0.1 :: Double) Statements.Sleep.statement
              Session.script "commit;"

        result `shouldBe` Nothing

        -- Connection should still be usable after timeout in transaction
        result <-
          Connection.use connection do
            Session.statement () Statements.SelectOne.statement

        result `shouldBe` Right 1

      it "Lets us start another transaction" do
        let checkTransactionStatus =
              Statement.Statement
                "select case when pg_advisory_lock(1) is null then 0 else 1 end"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                True
         in \config -> Scripts.onConnection config \connection -> do
              -- Timeout during a transaction
              result <-
                timeout 50_000 do
                  Connection.use connection do
                    Session.script "begin;"
                    Session.statement (0.1 :: Double) Statements.Sleep.statement

              result `shouldBe` Nothing

              -- Verify we can start a new transaction without "already in progress" error
              result <-
                Connection.use connection do
                  Session.script "begin;"
                  s <- Session.statement () checkTransactionStatus
                  Session.script "commit;"
                  return s

              result `shouldBe` Right 1

      it "Does not corrupt the prepared statement registry" do
        let returnIntStatement =
              Statement.Statement
                "select $1::int"
                (Encoders.param (Encoders.nonNullable Encoders.int4))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                True
         in \config -> Scripts.onConnection config \connection -> do
              -- Use a prepared statement first
              result <-
                Connection.use connection do
                  Session.statement 42 returnIntStatement

              result `shouldBe` Right 42

              -- Timeout during transaction (causes connection reset)
              result <-
                timeout 50_000 do
                  Connection.use connection do
                    Session.script "begin;"
                    Session.statement (0.1 :: Double) Statements.Sleep.statement
                    Session.script "commit;"

              result `shouldBe` Nothing

              -- The prepared statement should work again without "does not exist" error
              result <-
                Connection.use connection do
                  Session.statement 99 returnIntStatement

              result `shouldBe` Right 99
