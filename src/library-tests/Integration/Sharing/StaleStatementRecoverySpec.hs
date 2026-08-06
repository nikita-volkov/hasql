module Integration.Sharing.StaleStatementRecoverySpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Hasql.Decoders qualified as Decoders
import Hasql.Errors qualified as Errors
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Prelude
import Test.Hspec

spec :: SpecWith Scripts.ScopeParams
spec = do
  describe "Cached plan invalidated by a migration" do
    it "recovers outside of a transaction" \config -> do
      onEagerConnection config \connection -> do
        table <- Scripts.generateSymname
        result <- Connection.use connection do
          Session.script ("create table " <> table <> " (a int4)")
          -- Gets the statement prepared, and its result type recorded.
          _ <- Session.statement () (selectAll table)
          Session.script ("alter table " <> table <> " add column b int4")
          Session.statement () (selectAll table)
        result `shouldBe` Right ()

    it "does not leave the entry behind, so the next execution prepares afresh" \config -> do
      onEagerConnection config \connection -> do
        table <- Scripts.generateSymname
        stats <- Connection.use connection do
          Session.script ("create table " <> table <> " (a int4)")
          _ <- Session.statement () (selectAll table)
          Session.script ("alter table " <> table <> " add column b int4")
          _ <- Session.statement () (selectAll table)
          _ <- Session.statement () (selectAll table)
          Session.statementCacheStats
        fmap Session.admissions stats `shouldBe` Right 2

    it "gives up inside an open transaction, which the error has already poisoned" \config -> do
      onEagerConnection config \connection -> do
        table <- Scripts.generateSymname
        _ <- Connection.use connection do
          Session.script ("create table " <> table <> " (a int4)")
          Session.statement () (selectAll table)
        result <- Connection.use connection do
          Session.script "begin"
          Session.script ("alter table " <> table <> " add column b int4")
          Session.statement () (selectAll table)
        result `shouldSatisfy` \case
          Left (Errors.StatementSessionError _ _ _ _ _ (Errors.ServerStatementError (Errors.ServerError code _ _ _ _))) ->
            code == "0A000"
          _ -> False

  describe "Prepared statements dropped behind the driver's back" do
    it "recovers outside of a transaction" \config -> do
      onEagerConnection config \connection -> do
        marker <- Scripts.generateSymname
        result <- Connection.use connection do
          _ <- Session.statement () (marked marker)
          Session.script "DEALLOCATE ALL"
          Session.statement () (marked marker)
        result `shouldBe` Right 1

    it "drops the whole set rather than one entry at a time" \config -> do
      -- Otherwise every cached statement would cost one spurious error.
      onEagerConnection config \connection -> do
        markerA <- Scripts.generateSymname
        markerB <- Scripts.generateSymname
        result <- Connection.use connection do
          _ <- Session.statement () (marked markerA)
          _ <- Session.statement () (marked markerB)
          Session.script "DEALLOCATE ALL"
          _ <- Session.statement () (marked markerA)
          Session.statement () (marked markerB)
        result `shouldBe` Right 1

    it "gives up inside an open transaction" \config -> do
      onEagerConnection config \connection -> do
        marker <- Scripts.generateSymname
        _ <- Connection.use connection (Session.statement () (marked marker))
        result <- Connection.use connection do
          Session.script "DEALLOCATE ALL"
          Session.script "begin"
          Session.statement () (marked marker)
        result `shouldSatisfy` \case
          Left (Errors.StatementSessionError _ _ _ _ _ (Errors.ServerStatementError (Errors.ServerError code _ _ _ _))) ->
            code == "26000"
          _ -> False

-- | Connection that prepares on first use, so a single execution is enough to
-- set up a stale entry.
onEagerConnection :: Scripts.ScopeParams -> (Connection.Connection -> IO a) -> IO a
onEagerConnection =
  Scripts.onConnection (Settings.statementCacheSize 10 <> Settings.prepareThreshold 1)

-- | Statement whose result type changes when a column is added to the table.
selectAll :: Text -> Statement.Statement () ()
selectAll table =
  Statement.statement ("select * from " <> table) mempty Decoders.noResult

marked :: Text -> Statement.Statement () Int32
marked marker =
  Statement.statement
    ("select 1 -- " <> marker)
    mempty
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
