module Sharing.Functionality.Session.ScriptSpec (spec) where

import Data.Either
import Hasql.Connection qualified as Connection
import Hasql.Connection.Location qualified as Connection.Location
import Hasql.Session qualified as Session
import Test.Hspec
import Prelude

spec :: SpecWith Connection.Connection
spec = do
  it "returns ServerUsageError on syntax errors" \connection -> do
    result <- Connection.use connection (Session.script "THIS IS INVALID SQL")
    case result of
      Left (Connection.ServerUsageError (Connection.Location.ScriptInStatementOrScript _) _ _ _ _ _) -> pure ()
      _ -> expectationFailure $ "Expected ServerUsageError in Script, got: " <> show result
