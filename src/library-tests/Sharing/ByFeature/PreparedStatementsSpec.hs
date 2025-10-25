module Sharing.ByFeature.PreparedStatementsSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Dsls.Execution qualified as Execution
import Helpers.Scripts qualified as Scripts
import Helpers.Statements.CountPreparedStatements qualified as CountPreparedStatements
import Test.Hspec
import Prelude

spec :: SpecWith (Text, Word16)
spec = do
  describe "Prepared statements" do
    it "Do get prepared when configuration allows" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        -- Execute a preparable statement
        result <-
          Connection.use connection do
            Session.statement
              ()
              ( Statement.Statement
                  "select 1 + 1"
                  mempty
                  (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                  True
              )
        result `shouldBe` Right 2

        -- Query pg_prepared_statements to verify it was prepared
        preparedCount <-
          Connection.use connection do
            Execution.sessionByParams CountPreparedStatements.CountPreparedStatements

        preparedCount `shouldSatisfy` \case
          Right count -> count > 0
          Left _ -> False

    it "Do not get prepared when configuration forbids it" \config -> do
      Scripts.onUnpreparableConnection config \connection -> do
        -- Execute a statement marked as preparable
        result <-
          Connection.use connection do
            Session.statement
              ()
              ( Statement.Statement
                  "select 2 + 2"
                  mempty
                  (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                  True
              )
        result `shouldBe` Right 4

        -- Query pg_prepared_statements to verify it was NOT prepared
        preparedCount <-
          Connection.use connection do
            Execution.sessionByParams CountPreparedStatements.CountPreparedStatements

        preparedCount `shouldBe` Right 0
