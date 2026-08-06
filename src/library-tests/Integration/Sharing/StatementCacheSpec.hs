module Integration.Sharing.StatementCacheSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Hasql.Decoders qualified as Decoders
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.PreparedStatements qualified as PreparedStatements
import Helpers.Scripts qualified as Scripts
import Prelude
import Test.Hspec

spec :: SpecWith Scripts.ScopeParams
spec = do
  describe "Admission" do
    it "prepares a statement only on the execution that reaches the threshold" \config -> do
      onCache 10 3 config \connection -> do
        marker <- Scripts.generateSymname
        let stmt = markedStatement marker
        counts <-
          expectRight
            =<< Connection.use connection do
              for [1 .. 4 :: Int] \_ -> do
                _ <- Session.statement () stmt
                PreparedStatements.countPrepared
        counts `shouldBe` [0, 0, 1, 1]

    it "prepares nothing at all when the cache is disabled" \config -> do
      onCache 0 1 config \connection -> do
        marker <- Scripts.generateSymname
        result <-
          expectRight
            =<< Connection.use connection do
              replicateM_ 5 (Session.statement () (markedStatement marker))
              (,) <$> PreparedStatements.countPrepared <*> Session.statementCacheStats
        fst result `shouldBe` 0
        Session.size (snd result) `shouldBe` 0
        Session.admissions (snd result) `shouldBe` 0

  describe "Eviction" do
    it "keeps the server-side set within the configured size" \config -> do
      onCache 2 2 config \connection -> do
        markers <- replicateM 5 Scripts.generateSymname
        counts <-
          expectRight
            =<< Connection.use connection do
              for markers \marker -> do
                -- Twice, so that each one crosses the threshold.
                replicateM_ 2 (Session.statement () (markedStatement marker))
                PreparedStatements.countPrepared
        counts `shouldBe` [1, 2, 2, 2, 2]

    it "deallocates the statement it displaces" \config -> do
      onCache 1 2 config \connection -> do
        first <- Scripts.generateSymname
        second <- Scripts.generateSymname
        result <-
          expectRight
            =<< Connection.use connection do
              replicateM_ 2 (Session.statement () (markedStatement first))
              firstAfterFirst <- PreparedStatements.isPrepared first
              replicateM_ 2 (Session.statement () (markedStatement second))
              firstAfterSecond <- PreparedStatements.isPrepared first
              secondAfterSecond <- PreparedStatements.isPrepared second
              pure (firstAfterFirst, firstAfterSecond, secondAfterSecond)
        result `shouldBe` (True, False, True)

    it "survives evicting a statement used later in the same pipeline" \config -> do
      -- The statement evicted at one position simply misses at the later one
      -- and is prepared afresh under a new name.
      onCache 1 1 config \connection -> do
        a <- Scripts.generateSymname
        b <- Scripts.generateSymname
        result <-
          Connection.use connection do
            Session.pipeline do
              (,,)
                <$> Pipeline.statement () (markedStatement a)
                <*> Pipeline.statement () (markedStatement b)
                <*> Pipeline.statement () (markedStatement a)
        result `shouldBe` Right (1, 1, 1)

  describe "Stats" do
    it "account for hits, misses, admissions and evictions" \config -> do
      onCache 1 1 config \connection -> do
        a <- Scripts.generateSymname
        b <- Scripts.generateSymname
        stats <-
          expectRight
            =<< Connection.use connection do
              -- a: admitted. a: hit. b: admitted, evicting a.
              _ <- Session.statement () (markedStatement a)
              _ <- Session.statement () (markedStatement a)
              _ <- Session.statement () (markedStatement b)
              Session.statementCacheStats
        Session.size stats `shouldBe` 1
        Session.admissions stats `shouldBe` 2
        Session.hits stats `shouldBe` 1
        Session.misses stats `shouldBe` 2
        Session.evictions stats `shouldBe` 1

    it "are carried across uses of the connection" \config -> do
      onCache 10 1 config \connection -> do
        marker <- Scripts.generateSymname
        _ <- Connection.use connection (Session.statement () (markedStatement marker))
        stats <-
          expectRight
            =<< Connection.use connection do
              _ <- Session.statement () (markedStatement marker)
              Session.statementCacheStats
        Session.hits stats `shouldBe` 1
        Session.admissions stats `shouldBe` 1

-- | Connection with an explicitly configured statement cache.
onCache :: Int -> Int -> Scripts.ScopeParams -> (Connection.Connection -> IO a) -> IO a
onCache size threshold =
  Scripts.onConnection (Settings.statementCacheSize size <> Settings.prepareThreshold threshold)

-- | Statement uniquely identifiable in @pg_prepared_statements@ by its marker.
markedStatement :: Text -> Statement.Statement () Int32
markedStatement marker =
  Statement.statement
    ("select 1 -- " <> marker)
    mempty
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))

expectRight :: (Show e) => Either e a -> IO a
expectRight = \case
  Right a -> pure a
  Left err -> fail ("Unexpected error: " <> show err)
