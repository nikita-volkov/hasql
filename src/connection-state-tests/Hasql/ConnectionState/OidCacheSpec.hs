module Hasql.ConnectionState.OidCacheSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import CodecVocab.QualifiedTypeName qualified as CodecVocab.QualifiedTypeName
import CodecVocab.TypeInfo qualified as CodecVocab.TypeInfo
import Hasql.ConnectionState.OidCache qualified as OidCache
import Prelude
import Test.Hspec

int4Key :: CodecVocab.QualifiedTypeName.QualifiedTypeName
int4Key = CodecVocab.QualifiedTypeName.QualifiedTypeName Nothing "int4"

int8Key :: CodecVocab.QualifiedTypeName.QualifiedTypeName
int8Key = CodecVocab.QualifiedTypeName.QualifiedTypeName Nothing "int8"

spec :: Spec
spec = do
  describe "empty" do
    it "returns Nothing on lookup" do
      OidCache.lookupTypeInfo int4Key OidCache.empty
        `shouldBe` Nothing

  describe "fromHashMap and lookupTypeInfo" do
    it "can look up an inserted type" do
      let cache = OidCache.fromHashMap (HashMap.singleton int4Key (CodecVocab.TypeInfo.TypeInfo 23 1007))
      OidCache.lookupTypeInfo int4Key cache
        `shouldBe` Just (CodecVocab.TypeInfo.TypeInfo 23 1007)

    it "returns Nothing for a non-inserted type" do
      let cache = OidCache.fromHashMap (HashMap.singleton int4Key (CodecVocab.TypeInfo.TypeInfo 23 1007))
      OidCache.lookupTypeInfo int8Key cache
        `shouldBe` Nothing

    it "handles schema-qualified names" do
      let key = CodecVocab.QualifiedTypeName.QualifiedTypeName (Just "public") "my_type"
          cache = OidCache.fromHashMap (HashMap.singleton key (CodecVocab.TypeInfo.TypeInfo 100 200))
      OidCache.lookupTypeInfo key cache
        `shouldBe` Just (CodecVocab.TypeInfo.TypeInfo 100 200)
      OidCache.lookupTypeInfo (CodecVocab.QualifiedTypeName.QualifiedTypeName Nothing "my_type") cache
        `shouldBe` Nothing

    it "distinguishes same type name in different schemas" do
      let keyA = CodecVocab.QualifiedTypeName.QualifiedTypeName (Just "schema_a") "my_type"
          keyB = CodecVocab.QualifiedTypeName.QualifiedTypeName (Just "schema_b") "my_type"
          cache = OidCache.fromHashMap (HashMap.fromList [(keyA, CodecVocab.TypeInfo.TypeInfo 100 200), (keyB, CodecVocab.TypeInfo.TypeInfo 300 400)])
      OidCache.lookupTypeInfo keyA cache
        `shouldBe` Just (CodecVocab.TypeInfo.TypeInfo 100 200)
      OidCache.lookupTypeInfo keyB cache
        `shouldBe` Just (CodecVocab.TypeInfo.TypeInfo 300 400)

  describe "selectUnknownNames" do
    it "returns all names when cache is empty" do
      let names = HashSet.fromList [int4Key, int8Key]
      OidCache.selectUnknownNames names OidCache.empty
        `shouldBe` names

    it "returns empty when all names are known" do
      let cache = OidCache.fromHashMap (HashMap.fromList [(int4Key, CodecVocab.TypeInfo.TypeInfo 23 1007), (int8Key, CodecVocab.TypeInfo.TypeInfo 20 1016)])
          names = HashSet.fromList [int4Key, int8Key]
      OidCache.selectUnknownNames names cache
        `shouldBe` HashSet.empty

    it "returns only unknown names" do
      let cache = OidCache.fromHashMap (HashMap.singleton int4Key (CodecVocab.TypeInfo.TypeInfo 23 1007))
          names = HashSet.fromList [int4Key, int8Key]
      OidCache.selectUnknownNames names cache
        `shouldBe` HashSet.fromList [int8Key]

  describe "toResolver" do
    it "resolves a known type" do
      let cache = OidCache.fromHashMap (HashMap.singleton int4Key (CodecVocab.TypeInfo.TypeInfo 23 1007))
      OidCache.toResolver cache int4Key
        `shouldBe` CodecVocab.TypeInfo.TypeInfo 23 1007

    it "falls back to invalid for an unknown type" do
      OidCache.toResolver OidCache.empty int4Key
        `shouldBe` CodecVocab.TypeInfo.invalid

  describe "Semigroup" do
    it "right operand takes precedence for duplicate keys" do
      let cacheA = OidCache.fromHashMap (HashMap.singleton int4Key (CodecVocab.TypeInfo.TypeInfo 23 1007))
          cacheB = OidCache.fromHashMap (HashMap.singleton int4Key (CodecVocab.TypeInfo.TypeInfo 99 999))
          merged = cacheA <> cacheB
      OidCache.lookupTypeInfo int4Key merged
        `shouldBe` Just (CodecVocab.TypeInfo.TypeInfo 99 999)

    it "preserves entries from both sides when no conflict" do
      let cacheA = OidCache.fromHashMap (HashMap.singleton int4Key (CodecVocab.TypeInfo.TypeInfo 23 1007))
          cacheB = OidCache.fromHashMap (HashMap.singleton int8Key (CodecVocab.TypeInfo.TypeInfo 20 1016))
          merged = cacheA <> cacheB
      OidCache.lookupTypeInfo int4Key merged
        `shouldBe` Just (CodecVocab.TypeInfo.TypeInfo 23 1007)
      OidCache.lookupTypeInfo int8Key merged
        `shouldBe` Just (CodecVocab.TypeInfo.TypeInfo 20 1016)

    it "is associative" do
      let a = OidCache.fromHashMap (HashMap.singleton "t1" (CodecVocab.TypeInfo.TypeInfo 1 2))
          b = OidCache.fromHashMap (HashMap.fromList [("t1", CodecVocab.TypeInfo.TypeInfo 3 4), ("t2", CodecVocab.TypeInfo.TypeInfo 5 6)])
          c = OidCache.fromHashMap (HashMap.fromList [("t2", CodecVocab.TypeInfo.TypeInfo 7 8), ("t3", CodecVocab.TypeInfo.TypeInfo 9 10)])
      (a <> b) <> c
        `shouldBe` a <> (b <> c)

  describe "Monoid" do
    it "mempty is identity for Semigroup" do
      let cache = OidCache.fromHashMap (HashMap.singleton int4Key (CodecVocab.TypeInfo.TypeInfo 23 1007))
      cache <> mempty
        `shouldBe` cache
      mempty <> cache
        `shouldBe` cache

    it "empty equals mempty" do
      OidCache.empty
        `shouldBe` mempty
