module Sharing.ByUnit.Session.ScriptSpec (spec) where

import Data.Either
import Hasql.Connection qualified as Connection
import Hasql.Errors qualified as Errors
import Hasql.Session qualified as Session
import Helpers.Scripts qualified as Scripts
import Test.Hspec
import Prelude

spec :: SpecWith (Text, Word16)
spec = do
  it "returns ServerSessionError on syntax errors" \config -> do
    Scripts.onConnection config \connection -> do
      result <- Connection.use connection (Session.script "THIS IS INVALID SQL")
      case result of
        Left (Errors.ScriptSessionError _ _) -> pure ()
        _ -> expectationFailure $ "Expected ScriptSessionError with ExecutionScriptError, got: " <> show result
