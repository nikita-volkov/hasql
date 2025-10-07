module Sharing.Functionality.Session.SyntaxErrorsSpec (spec) where

import Data.Either
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Errors qualified as Errors
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Test.Hspec
import Prelude

spec :: SpecWith (Text, Word16)
spec = do
  forM_ [False, True] \inPipeline -> do
    describe (if inPipeline then "Pipeline" else "Session") do
      forM_ [False, True] \preparable -> do
        describe (if preparable then "Preparable" else "Unpreparable") do
          it "gets reported properly" \config -> Scripts.onConnection config \connection -> do
            result <- Connection.use connection do
              let statement = Statement.Statement "-" mempty Decoders.noResult preparable
              if inPipeline
                then Session.pipeline (Pipeline.statement () statement)
                else Session.statement () statement

            shouldBe
              result
              ( Left
                  ( (Errors.StatementSessionError 1 0 "-" [] preparable)
                      ( Errors.ExecutionStatementError
                          ( Errors.ExecutionError
                              "42601"
                              "syntax error at or near \"-\""
                              Nothing
                              Nothing
                              (Just 1)
                          )
                      )
                  )
              )
