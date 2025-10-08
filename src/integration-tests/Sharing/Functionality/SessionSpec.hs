module Sharing.Functionality.SessionSpec (spec) where

import Data.Either
import Hasql.Connection qualified as Connection
import Helpers.Dsls.Execution qualified as Execution
import Helpers.Scripts qualified as Scripts
import Helpers.Statements qualified as Statements
import Test.Hspec
import Prelude

spec :: SpecWith (Text, Word16)
spec = do
  it "Does not lose the server-side session state on timeout" \config -> do
    Scripts.onConnection config \connection -> do
      varname <- Execution.generateVarname
      result <- timeout 50_000 do
        Connection.use connection do
          Execution.sessionByParams (Statements.SetConfig varname "1" False)
          Execution.sessionByParams (Statements.Sleep 0.1)

      result `shouldBe` Nothing

      result <- Connection.use connection do
        Execution.sessionByParams (Statements.CurrentSetting varname True)

      result `shouldBe` Right (Just "1")

  it "Does not lose the server-side session state between uses" \config -> do
    Scripts.onConnection config \connection -> do
      varname <- Execution.generateVarname

      result <- Connection.use connection do
        Execution.sessionByParams (Statements.SetConfig varname "1" False)
      result `shouldSatisfy` isRight

      result <- Connection.use connection do
        Execution.sessionByParams (Statements.CurrentSetting varname True)
      result `shouldBe` Right (Just "1")
