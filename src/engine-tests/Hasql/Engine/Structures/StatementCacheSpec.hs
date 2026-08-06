module Hasql.Engine.Structures.StatementCacheSpec (spec) where

import Data.ByteString (ByteString)
import Data.List qualified as List
import Data.String (fromString)
import Hasql.Engine.Structures.StatementCache (Decision (..))
import Hasql.Engine.Structures.StatementCache qualified as StatementCache
import Prelude
import Test.Hspec

-- | Cache with room for everything that prepares on first use.
eagerCache :: StatementCache.StatementCache
eagerCache = StatementCache.empty 10 1

key :: ByteString -> StatementCache.LocalKey
key sql = StatementCache.localKey sql []

-- | Run a sequence of statements through the cache, collecting the decisions.
decideAll :: StatementCache.StatementCache -> [ByteString] -> ([Decision], StatementCache.StatementCache)
decideAll cache sqls =
  let (finalCache, decisions) =
        List.mapAccumL
          (\cache sql -> let (decision, newCache) = StatementCache.decide (key sql) cache in (newCache, decision))
          cache
          sqls
   in (decisions, finalCache)

spec :: Spec
spec = do
  describe "Admission" do
    it "does not prepare below the threshold" do
      let (decisions, _) = decideAll (StatementCache.empty 10 3) (replicate 2 "select 1")
      decisions `shouldBe` [MissDecision, MissDecision]

    it "prepares exactly at the threshold" do
      let (decisions, _) = decideAll (StatementCache.empty 10 3) (replicate 4 "select 1")
      decisions
        `shouldBe` [ MissDecision,
                     MissDecision,
                     AdmitDecision "0" Nothing,
                     HitDecision "0"
                   ]

    it "counts executions per statement rather than globally" do
      let (decisions, _) =
            decideAll (StatementCache.empty 10 2) ["a", "b", "c", "a"]
      decisions
        `shouldBe` [ MissDecision,
                     MissDecision,
                     MissDecision,
                     AdmitDecision "0" Nothing
                   ]

    it "prepares on first use at threshold 1" do
      let (decisions, _) = decideAll eagerCache ["select 1"]
      decisions `shouldBe` [AdmitDecision "0" Nothing]

    it "clamps a threshold below 1 up to 1" do
      let (decisions, _) = decideAll (StatementCache.empty 10 0) ["select 1"]
      decisions `shouldBe` [AdmitDecision "0" Nothing]

  describe "Identity" do
    it "distinguishes different SQL" do
      let (decisions, _) = decideAll eagerCache ["select 1", "select 2"]
      decisions `shouldBe` [AdmitDecision "0" Nothing, AdmitDecision "1" Nothing]

    it "distinguishes the same SQL under different parameter OIDs" do
      let (first, cache) = StatementCache.decide (StatementCache.localKey "select $1" [23]) eagerCache
          (second, _) = StatementCache.decide (StatementCache.localKey "select $1" [25]) cache
      first `shouldBe` AdmitDecision "0" Nothing
      second `shouldBe` AdmitDecision "1" Nothing

    it "distinguishes different parameter OID ordering" do
      let (first, cache) = StatementCache.decide (StatementCache.localKey "select $1, $2" [23, 25]) eagerCache
          (second, _) = StatementCache.decide (StatementCache.localKey "select $1, $2" [25, 23]) cache
      first `shouldBe` AdmitDecision "0" Nothing
      second `shouldBe` AdmitDecision "1" Nothing

    it "recognises the same SQL under the same parameter OIDs" do
      let sameKey = StatementCache.localKey "select $1, $2, $3" [23, 25, 1043]
          (_, cache) = StatementCache.decide sameKey eagerCache
          (second, _) = StatementCache.decide sameKey cache
      second `shouldBe` HitDecision "0"

  describe "Eviction" do
    it "reports the victim once at capacity" do
      let (decisions, _) = decideAll (StatementCache.empty 2 1) ["a", "b", "c"]
      decisions
        `shouldBe` [ AdmitDecision "0" Nothing,
                     AdmitDecision "1" Nothing,
                     AdmitDecision "2" (Just "0")
                   ]

    it "evicts the least recently used rather than the oldest" do
      -- "a" is admitted first but used again before "c" arrives, so "b" is
      -- the one that has to go.
      let (decisions, _) = decideAll (StatementCache.empty 2 1) ["a", "b", "a", "c"]
      decisions
        `shouldBe` [ AdmitDecision "0" Nothing,
                     AdmitDecision "1" Nothing,
                     HitDecision "0",
                     AdmitDecision "2" (Just "1")
                   ]

    it "never holds more than the capacity" do
      let (_, cache) = decideAll (StatementCache.empty 3 1) (fmap (fromString . show) [1 .. 20 :: Int])
      StatementCache.size (StatementCache.toStats cache) `shouldBe` 3

    it "keeps issuing fresh remote keys after eviction" do
      -- Reusing a name a survivor still holds would be a 42P05 on the server.
      let (decisions, _) = decideAll (StatementCache.empty 1 1) ["a", "b", "a"]
      decisions
        `shouldBe` [ AdmitDecision "0" Nothing,
                     AdmitDecision "1" (Just "0"),
                     AdmitDecision "2" (Just "1")
                   ]

  describe "Pending bounding" do
    it "never holds more pending entries than the capacity" do
      -- A stream of unique SQL must not be able to grow the cache without
      -- bound just by never getting admitted.
      let (_, cache) = decideAll (StatementCache.empty 3 100) (fmap (fromString . show) [1 .. 500 :: Int])
          stats = StatementCache.toStats cache
      StatementCache.size stats `shouldBe` 0
      StatementCache.admissions stats `shouldBe` 0

    it "does not let a stream of unique SQL displace an admitted statement" do
      let (_, cache) = StatementCache.decide (key "hot") (StatementCache.empty 3 2)
          (_, warmed) = StatementCache.decide (key "hot") cache
          (_, polluted) = decideAll warmed (fmap (fromString . show) [1 .. 100 :: Int])
          (decision, _) = StatementCache.decide (key "hot") polluted
      decision `shouldBe` HitDecision "0"

  describe "Disabled cache" do
    it "prepares nothing at size 0" do
      let (decisions, _) = decideAll (StatementCache.empty 0 1) (replicate 5 "select 1")
      decisions `shouldBe` replicate 5 MissDecision

    it "clamps a negative size up to 0" do
      let (decisions, _) = decideAll (StatementCache.empty (-5) 1) (replicate 3 "select 1")
      decisions `shouldBe` replicate 3 MissDecision

    it "does not accumulate pending state at size 0" do
      -- Counting usage would be pointless work, and unbounded, when nothing
      -- can ever be admitted. A hundred distinct statements must therefore
      -- leave the cache in exactly the state a hundred repeats of one do.
      let (_, distinct) = decideAll (StatementCache.empty 0 2) (fmap (fromString . show) [1 .. 100 :: Int])
          (_, repeated) = decideAll (StatementCache.empty 0 2) (replicate 100 "x")
      distinct `shouldBe` repeated
      StatementCache.size (StatementCache.toStats distinct) `shouldBe` 0
      StatementCache.misses (StatementCache.toStats distinct) `shouldBe` 100

  describe "Invalidation" do
    it "evict drops only the named statement" do
      let (_, cache) = decideAll eagerCache ["a", "b"]
          evicted = StatementCache.evict (key "a") cache
          (decisionA, afterA) = StatementCache.decide (key "a") evicted
          (decisionB, _) = StatementCache.decide (key "b") afterA
      decisionA `shouldBe` AdmitDecision "2" Nothing
      decisionB `shouldBe` HitDecision "1"

    it "flush drops everything without reusing remote keys" do
      -- The server may still hold statements the flush assumed were gone, so
      -- reusing their names would collide.
      let (_, cache) = decideAll eagerCache ["a", "b"]
          (decisions, _) = decideAll (StatementCache.flush cache) ["a", "b"]
      decisions `shouldBe` [AdmitDecision "2" Nothing, AdmitDecision "3" Nothing]

    it "reset restarts the remote keys, as only a DEALLOCATE ALL permits" do
      let (_, cache) = decideAll eagerCache ["a", "b"]
          (decisions, _) = decideAll (StatementCache.reset cache) ["a"]
      decisions `shouldBe` [AdmitDecision "0" Nothing]

    it "reset preserves the configuration" do
      let (_, cache) = decideAll (StatementCache.empty 1 1) ["a", "b"]
          (decisions, _) = decideAll (StatementCache.reset cache) ["x", "y"]
      decisions `shouldBe` [AdmitDecision "0" Nothing, AdmitDecision "1" (Just "0")]

    it "withdraw undoes an admission without disturbing the stats" do
      let (_, cache) = decideAll eagerCache ["a"]
          withdrawn = StatementCache.withdraw (key "a") cache
      StatementCache.size (StatementCache.toStats withdrawn) `shouldBe` 0
      StatementCache.evictions (StatementCache.toStats withdrawn) `shouldBe` 0

  describe "Desync" do
    it "is not flagged on a fresh cache" do
      StatementCache.isDesynced eagerCache `shouldBe` False

    it "is flagged and cleared by a reset" do
      let (_, cache) = decideAll eagerCache ["a"]
          desynced = StatementCache.markDesynced cache
      StatementCache.isDesynced desynced `shouldBe` True
      StatementCache.size (StatementCache.toStats desynced) `shouldBe` 0
      StatementCache.isDesynced (StatementCache.reset desynced) `shouldBe` False

  describe "Stats" do
    it "accounts for every execution as either a hit or a miss" do
      let (_, cache) = decideAll (StatementCache.empty 10 2) ["a", "a", "a", "b"]
          stats = StatementCache.toStats cache
      StatementCache.hits stats `shouldBe` 1
      StatementCache.misses stats `shouldBe` 3
      StatementCache.admissions stats `shouldBe` 1
      StatementCache.size stats `shouldBe` 1

    it "counts evictions" do
      let (_, cache) = decideAll (StatementCache.empty 1 1) ["a", "b", "c"]
      StatementCache.evictions (StatementCache.toStats cache) `shouldBe` 2
