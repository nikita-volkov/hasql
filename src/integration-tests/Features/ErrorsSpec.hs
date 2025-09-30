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

  describe "Multiple errors in sequence" do
    describe "In Session" do
      it "Get captured correctly" $ \connection -> do
        result1 <-
          Connection.use
            connection
            ( Session.statement
                ()
                ( Statement.Statement
                    "INVALID SQL SYNTAX HERE"
                    Encoders.noParams
                    Decoders.noResult
                    False
                )
            )
        result2 <-
          Connection.use
            connection
            ( Session.statement
                ()
                ( Statement.Statement
                    "SELECT 1/0"
                    Encoders.noParams
                    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                    False
                )
            )

        -- All should be errors of the expected types
        case (result1, result2) of
          ( Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "42601" _ _ _ _))),
            Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "22012" _ _ _ _)))
            ) ->
              pure ()
          _ -> expectationFailure ("Expected both error types, got: " <> show (result1, result2))

        -- Connection should still be usable after errors
        result3 <- Connection.use connection (Session.sql "SELECT 1")
        result3 `shouldBe` Right ()
