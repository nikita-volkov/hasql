module Features.ErrorsSpec (spec) where

import Data.Either
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec
import TestingKit.Testcontainers qualified as Testcontainers
import Prelude

spec :: Spec
spec = Testcontainers.aroundSpecWithConnection False do
  it "recovers after catch within session" \connection -> do
    let tryStatement =
          Statement.Statement
            "select $1 :: int8"
            (Encoders.param (Encoders.nonNullable Encoders.int8))
            (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
            True

    result <-
      Connection.use connection do
        -- First successful query
        a <- Session.statement (1 :: Int64) tryStatement
        -- This should fail but connection should remain usable
        () <- catchError (Session.sql "absurd") (const (pure ()))
        -- Second successful query
        b <- Session.statement (2 :: Int64) tryStatement
        pure (a, b)

    result `shouldBe` Right (1, 2)

  describe "Transactions" do
    it "do not cause \"in progress after error\"" \connection -> do
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
          Session.sql "."

      result `shouldSatisfy` isLeft

      result <-
        Connection.use connection do
          Session.sql "begin;"
          s <- Session.statement (1 :: Int64, 2 :: Int64) sumStatement
          Session.sql "end;"
          return s

      result `shouldBe` Right (3 :: Int64)
