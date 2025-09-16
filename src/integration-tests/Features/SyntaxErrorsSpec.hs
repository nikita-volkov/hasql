module Features.SyntaxErrorsSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec
import TestingKit.Testcontainers qualified as Testcontainers
import Prelude
import Data.Text qualified as Text

spec :: Spec
spec = Testcontainers.aroundSpecWithConnection True do
  describe "postgres:17" do
    describe "Pipeline" do
      describe "Preparable" do
        it "gets reported properly" \connection -> do
          result <- Connection.use connection do
            Session.pipeline do
              Pipeline.statement
                ()
                ( Statement.Statement
                    "-"  -- This should cause a syntax error
                    mempty
                    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                    True  -- preparable = True
                )
          -- We expect a FatalError with code 42601 (syntax error), not a PipelineAbort
          case result of
            Left (Session.QueryError "-" [] (Session.ResultError (Session.ServerError "FatalError" "42601" _ _ _ _))) ->
              pure ()
            Left (Session.QueryError "-" [] (Session.ResultError (Session.UnexpectedResult msg))) | "PipelineAbort" `Text.isInfixOf` msg ->
              expectationFailure $ "Got PipelineAbort instead of FatalError: " <> show result
            _ ->
              expectationFailure $ "Unexpected result: " <> show result