module Features.SyntaxErrorsSpec (spec) where

import Data.Either
import Hasql.Connection qualified as Connection
import Hasql.Connection.Location qualified as Location
import Hasql.Decoders qualified as Decoders
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec
import TestingKit.Testcontainers qualified as Testcontainers
import Prelude

spec :: Spec
spec = Testcontainers.aroundSpecWithConnection True do
  forM_ [False, True] \inPipeline -> do
    describe (if inPipeline then "Pipeline" else "Session") do
      forM_ [False, True] \preparable -> do
        describe (if preparable then "Preparable" else "Unpreparable") do
          it "gets reported properly" \connection -> do
            result <- Connection.use connection do
              let statement = Statement.Statement "-" mempty Decoders.noResult preparable
              if inPipeline
                then Session.pipeline (Pipeline.statement () statement)
                else Session.statement () statement

            shouldBe
              result
              ( Left
                  ( Connection.ServerUsageError
                      (Location.StatementInStatementOrScript (Location.InStatement 1 0 "-" [] preparable))
                      "42601"
                      "syntax error at or near \"-\""
                      Nothing
                      Nothing
                      (Just 1)
                  )
              )
