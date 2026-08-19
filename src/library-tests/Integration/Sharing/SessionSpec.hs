module Integration.Sharing.SessionSpec (spec) where

import Data.Either
import Hasql.Connection qualified as Connection
import Helpers.Dsls.Execution qualified as Execution
import Helpers.Scripts qualified as Scripts
import Helpers.Statements qualified as Statements
import Prelude
import Test.Hspec

spec :: SpecWith Scripts.ScopeParams
spec = do
  -- Note the guarantee this does not extend to: an interruption takes the
  -- connection with it, and server-side session state goes with the
  -- connection. See "Integration.Sharing.Connection.Use.InterruptionSpec".
  it "Does not lose the server-side session state between uses" \config -> do
    Scripts.onPreparableConnection config \connection -> do
      varname <- Execution.generateVarname

      result <- Connection.use connection do
        Execution.sessionByParams (Statements.SetConfig varname "1" False)
      result `shouldSatisfy` isRight

      result <- Connection.use connection do
        Execution.sessionByParams (Statements.CurrentSetting varname True)
      result `shouldBe` Right (Just "1")
