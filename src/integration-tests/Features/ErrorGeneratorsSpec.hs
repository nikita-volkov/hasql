module Features.ErrorGeneratorsSpec (spec) where

import Data.Either
import Hasql.Connection qualified as Connection
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Test.Hspec
import TestingKit.Statements.ErrorGenerators qualified as Errors
import TestingKit.Testcontainers qualified as Testcontainers
import Prelude

spec :: Spec
spec = Testcontainers.aroundSpecWithConnection False do
  describe "Error Generator Utilities" do
    describe "ServerError generators" do
      forM_ [False, True] $ \preparable -> do
        describe (if preparable then "Prepared" else "Unprepared") do
          it "generates syntax errors (42601)" $ \connection -> do
            result <- Connection.use connection (Errors.runSyntaxError preparable)
            case result of
              Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "42601" _ _ _ _))) ->
                pure ()
              _ -> expectationFailure ("Expected syntax error, got: " <> show result)

          it "generates relation not found errors (42P01)" $ \connection -> do
            result <- Connection.use connection (Errors.runRelationNotFound preparable)
            case result of
              Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "42P01" _ _ _ _))) ->
                pure ()
              _ -> expectationFailure ("Expected relation not found error, got: " <> show result)

          it "generates division by zero errors (22012)" $ \connection -> do
            result <- Connection.use connection (Errors.runDivisionByZero preparable)
            case result of
              Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "22012" _ _ _ _))) ->
                pure ()
              _ -> expectationFailure ("Expected division by zero error, got: " <> show result)

    describe "Parametric error generators" do
      forM_ [False, True] $ \preparable -> do
        describe (if preparable then "Prepared" else "Unprepared") do
          it "generates division by zero with parameters" $ \connection -> do
            let params = Errors.DivisionParams {dividend = 10, divisor = 0}
            result <- Connection.use connection (Errors.runParametricDivision preparable params)
            case result of
              Left (Session.QueryError _ renderedParams (Session.ResultError (Session.ServerError "22012" _ _ _ _))) -> do
                renderedParams `shouldContain` ["10"]
                renderedParams `shouldContain` ["0"]
              _ -> expectationFailure ("Expected division by zero with parameters, got: " <> show result)

          it "generates successful division with non-zero divisor" $ \connection -> do
            let params = Errors.DivisionParams {dividend = 10, divisor = 2}
            result <- Connection.use connection (Errors.runParametricDivision preparable params)
            result `shouldBe` Right 5

    describe "Type mismatch generators" do
      forM_ [False, True] $ \preparable -> do
        describe (if preparable then "Prepared" else "Unprepared") do
          it "generates text as int type mismatch" $ \connection -> do
            result <- Connection.use connection (Session.statement () (Errors.textAsIntStatement preparable))
            case result of
              Left (Session.QueryError _ _ (Session.ResultError (Session.DecoderTypeMismatch 0 _ _))) ->
                pure ()
              _ -> expectationFailure ("Expected DecoderTypeMismatch, got: " <> show result)

    describe "Pipeline error generators" do
      forM_ [False, True] $ \preparable -> do
        describe (if preparable then "Prepared" else "Unprepared") do
          it "generates syntax errors in pipelines" $ \connection -> do
            result <- Connection.use connection (Session.pipeline (Pipeline.statement () (Errors.syntaxErrorStatement preparable)))
            case result of
              Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "42601" _ _ _ _))) ->
                pure ()
              _ -> expectationFailure ("Expected pipeline syntax error, got: " <> show result)

  describe "Error generator combinations" do
    it "can generate multiple different errors in sequence" $ \connection -> do
      -- Test that error generators work reliably in sequence
      result1 <- Connection.use connection (Errors.runSyntaxError False)
      result2 <- Connection.use connection (Errors.runDivisionByZero False)

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
