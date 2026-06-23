module Hasql.Engine.Structures.StatementCacheSpec (spec) where

import Data.Maybe
import Database.PostgreSQL.LibPQ (Oid (..))
import Hasql.Engine.Structures.StatementCache qualified as StatementCache
import Test.Hspec

spec :: Spec
spec = do
  describe "empty" do
    it "returns Nothing on lookup" do
      StatementCache.lookup "SELECT 1" [] StatementCache.empty
        `shouldBe` Nothing

  describe "insert and lookup" do
    it "can insert and retrieve a statement" do
      let (remoteKey, cache) = StatementCache.insert "SELECT 1" [] StatementCache.empty
      StatementCache.lookup "SELECT 1" [] cache
        `shouldBe` Just remoteKey

    it "generates unique remote keys for different SQL" do
      let (key1, cache1) = StatementCache.insert "SELECT 1" [] StatementCache.empty
          (key2, _cache2) = StatementCache.insert "SELECT 2" [] cache1
      key1 `shouldNotBe` key2

    it "generates unique remote keys for same SQL with different OIDs" do
      let oid23 = Oid 23
          oid25 = Oid 25
          (key1, cache1) = StatementCache.insert "SELECT $1" [oid23] StatementCache.empty
          (key2, _cache2) = StatementCache.insert "SELECT $1" [oid25] cache1
      key1 `shouldNotBe` key2

    it "distinguishes statements with same SQL but different OIDs" do
      let oid23 = Oid 23
          oid25 = Oid 25
          (_key1, cache1) = StatementCache.insert "SELECT $1" [oid23] StatementCache.empty
          (_key2, cache2) = StatementCache.insert "SELECT $1" [oid25] cache1
      -- Both should be findable
      StatementCache.lookup "SELECT $1" [oid23] cache2
        `shouldSatisfy` isJust
      StatementCache.lookup "SELECT $1" [oid25] cache2
        `shouldSatisfy` isJust
      -- And should have different remote keys
      let rk1 = StatementCache.lookup "SELECT $1" [oid23] cache2
          rk2 = StatementCache.lookup "SELECT $1" [oid25] cache2
      rk1 `shouldNotBe` rk2

    it "returns Nothing for a non-inserted SQL" do
      let (_key, cache) = StatementCache.insert "SELECT 1" [] StatementCache.empty
      StatementCache.lookup "SELECT 2" [] cache
        `shouldBe` Nothing

    it "returns Nothing for matching SQL but different OIDs" do
      let oid23 = Oid 23
          oid25 = Oid 25
          (_key, cache) = StatementCache.insert "SELECT $1" [oid23] StatementCache.empty
      StatementCache.lookup "SELECT $1" [oid25] cache
        `shouldBe` Nothing

    it "handles empty OID list" do
      let (key, cache) = StatementCache.insert "SELECT 1" [] StatementCache.empty
      StatementCache.lookup "SELECT 1" [] cache
        `shouldBe` Just key

    it "handles multiple OIDs" do
      let oids = [Oid 23, Oid 25, Oid 1043]
          (key, cache) = StatementCache.insert "SELECT $1, $2, $3" oids StatementCache.empty
      StatementCache.lookup "SELECT $1, $2, $3" oids cache
        `shouldBe` Just key

    it "distinguishes different OID ordering" do
      let oidsA = [Oid 23, Oid 25]
          oidsB = [Oid 25, Oid 23]
          (_keyA, cache1) = StatementCache.insert "SELECT $1, $2" oidsA StatementCache.empty
          (_keyB, cache2) = StatementCache.insert "SELECT $1, $2" oidsB cache1
      StatementCache.lookup "SELECT $1, $2" oidsA cache2
        `shouldSatisfy` isJust
      StatementCache.lookup "SELECT $1, $2" oidsB cache2
        `shouldSatisfy` isJust
      let rkA = StatementCache.lookup "SELECT $1, $2" oidsA cache2
          rkB = StatementCache.lookup "SELECT $1, $2" oidsB cache2
      rkA `shouldNotBe` rkB

  describe "reset" do
    it "clears all cached statements" do
      let (_key, cache) = StatementCache.insert "SELECT 1" [] StatementCache.empty
          resetCache = StatementCache.reset cache
      StatementCache.lookup "SELECT 1" [] resetCache
        `shouldBe` Nothing

    it "results in a cache equal to empty" do
      let (_key, cache) = StatementCache.insert "SELECT 1" [] StatementCache.empty
      StatementCache.reset cache
        `shouldBe` StatementCache.empty
