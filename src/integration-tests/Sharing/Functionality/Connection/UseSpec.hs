module Sharing.Functionality.Connection.UseSpec (spec) where

import Data.Either
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
