module Hasql.ConnectionState.StatementCacheSpec (spec) where

import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Maybe
import Hasql.ConnectionState.StatementCache qualified as StatementCache
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  describe "remote key format" do
    it "is prefixed with \"hasql_\"" do
      let (key, _) = StatementCache.insert "SELECT 1" [] StatementCache.empty
      ByteString.Char8.isPrefixOf "hasql_" key `shouldBe` True

    it "is 63 bytes long" do
      let (key, _) = StatementCache.insert "SELECT 1" [] StatementCache.empty
      ByteString.length key `shouldBe` 63

    it "is deterministic across independent caches" do
      let (key1, _) = StatementCache.insert "SELECT 1" [] StatementCache.empty
          (key2, _) = StatementCache.insert "SELECT 1" [] StatementCache.empty
      key1 `shouldBe` key2

    it "is deterministic for statements with parameter OIDs" do
      let (key1, _) = StatementCache.insert "SELECT $1" [23] StatementCache.empty
          (key2, _) = StatementCache.insert "SELECT $1" [23] StatementCache.empty
      key1 `shouldBe` key2

    it "distinguishes statements by their parameter OIDs" do
      let (key1, _) = StatementCache.insert "SELECT $1" [23] StatementCache.empty
          (key2, _) = StatementCache.insert "SELECT $1" [25] StatementCache.empty
      key1 `shouldNotBe` key2

    it "distinguishes statements by their parameter count" do
      let (key1, _) = StatementCache.insert "SELECT 1" [] StatementCache.empty
          (key2, _) = StatementCache.insert "SELECT 1" [23] StatementCache.empty
      key1 `shouldNotBe` key2

    -- Golden values. The name is what the server sees and what a pooler
    -- tracks statements by, so changing any of it — the serialization, the
    -- digest, the truncation, the prefix — is a wire-visible change that
    -- needs a version bump and a changelog entry. Do not update these to
    -- make a build pass; work out first why the name moved.
    --
    -- Serialization under test: the SQL length as a big-endian Word64, the
    -- SQL, the parameter count as a big-endian Word64, then each OID as a
    -- big-endian Word32. SHA-256 of that, hex, first 57 characters, prefixed
    -- with "hasql_".
    it "matches the golden name for a statement without parameters" do
      let (key, _) = StatementCache.insert "SELECT 1" [] StatementCache.empty
      key `shouldBe` "hasql_5edac06ba8f1fb3b8ad72465f3cffb0adb965e440bcd7712e33606b97"

    it "matches the golden name for a statement with parameters" do
      let (key, _) = StatementCache.insert "SELECT $1" [23] StatementCache.empty
      key `shouldBe` "hasql_7497628d7431d49f8722fb860c35ebdc96c758a01032b58812c38ecaf"

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
      let oid23 = 23
          oid25 = 25
          (key1, cache1) = StatementCache.insert "SELECT $1" [oid23] StatementCache.empty
          (key2, _cache2) = StatementCache.insert "SELECT $1" [oid25] cache1
      key1 `shouldNotBe` key2

    it "distinguishes statements with same SQL but different OIDs" do
      let oid23 = 23
          oid25 = 25
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
      let oid23 = 23
          oid25 = 25
          (_key, cache) = StatementCache.insert "SELECT $1" [oid23] StatementCache.empty
      StatementCache.lookup "SELECT $1" [oid25] cache
        `shouldBe` Nothing

    it "handles empty OID list" do
      let (key, cache) = StatementCache.insert "SELECT 1" [] StatementCache.empty
      StatementCache.lookup "SELECT 1" [] cache
        `shouldBe` Just key

    it "handles multiple OIDs" do
      let oids = [23, 25, 1043]
          (key, cache) = StatementCache.insert "SELECT $1, $2, $3" oids StatementCache.empty
      StatementCache.lookup "SELECT $1, $2, $3" oids cache
        `shouldBe` Just key

    it "distinguishes different OID ordering" do
      let oidsA = [23, 25]
          oidsB = [25, 23]
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
