module Sharing.ByFeature.SyntaxErrorsSpec (spec) where

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
      forM_ [(False, Scripts.onNonPreparingConnection), (True, Scripts.onPreparingConnection)] \(prepared, onConnection) -> do
        describe (if prepared then "Prepared" else "Unprepared") do
          it "gets reported properly" \config -> do
            onConnection config \connection -> do
              result <- Connection.use connection do
                let statement = Statement.statement "-" mempty Decoders.noResult
                if inPipeline
                  then Session.pipeline (Pipeline.statement () statement)
                  else Session.statement () statement

              shouldBe
                result
                ( Left
                    ( (Errors.StatementSessionError 1 0 "-" [] prepared)
                        ( Errors.ServerStatementError
                            ( Errors.ServerError
                                "42601"
                                "syntax error at or near \"-\""
                                Nothing
                                Nothing
                                (Just 1)
                            )
                        )
                    )
                )
