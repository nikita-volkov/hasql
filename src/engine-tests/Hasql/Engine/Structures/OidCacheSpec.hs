module Hasql.Engine.Structures.OidCacheSpec (spec) where

import Data.HashSet qualified as HashSet
import Hasql.Engine.Structures.OidCache qualified as OidCache
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  describe "empty" do
    it "returns Nothing on scalar lookup" do
      OidCache.lookupScalar Nothing "int4" OidCache.empty
        `shouldBe` Nothing

    it "returns Nothing on array lookup" do
      OidCache.lookupArray Nothing "int4" OidCache.empty
        `shouldBe` Nothing

  describe "insertScalar and lookup" do
    it "can insert and lookup a scalar OID" do
      let cache = OidCache.insertScalar Nothing "int4" 23 1007 OidCache.empty
      OidCache.lookupScalar Nothing "int4" cache
        `shouldBe` Just 23

    it "can insert and lookup an array OID" do
      let cache = OidCache.insertScalar Nothing "int4" 23 1007 OidCache.empty
      OidCache.lookupArray Nothing "int4" cache
        `shouldBe` Just 1007

    it "returns Nothing for a non-inserted type" do
      let cache = OidCache.insertScalar Nothing "int4" 23 1007 OidCache.empty
      OidCache.lookupScalar Nothing "int8" cache
        `shouldBe` Nothing

    it "handles schema-qualified names" do
      let cache = OidCache.insertScalar (Just "public") "my_type" 100 200 OidCache.empty
      OidCache.lookupScalar (Just "public") "my_type" cache
        `shouldBe` Just 100
      OidCache.lookupScalar Nothing "my_type" cache
        `shouldBe` Nothing

    it "distinguishes same type name in different schemas" do
      let cache =
            OidCache.insertScalar
              (Just "schema_a")
              "my_type"
              100
              200
              (OidCache.insertScalar (Just "schema_b") "my_type" 300 400 OidCache.empty)
      OidCache.lookupScalar (Just "schema_a") "my_type" cache
        `shouldBe` Just 100
      OidCache.lookupScalar (Just "schema_b") "my_type" cache
        `shouldBe` Just 300

  describe "selectUnknownNames" do
    it "returns all names when cache is empty" do
      let names = HashSet.fromList [(Nothing, "int4"), (Nothing, "int8")]
      OidCache.selectUnknownNames names OidCache.empty
        `shouldBe` names

    it "returns empty when all names are known" do
      let cache =
            OidCache.insertScalar
              Nothing
              "int4"
              23
              1007
              (OidCache.insertScalar Nothing "int8" 20 1016 OidCache.empty)
          names = HashSet.fromList [(Nothing, "int4"), (Nothing, "int8")]
      OidCache.selectUnknownNames names cache
        `shouldBe` HashSet.empty

    it "returns only unknown names" do
      let cache = OidCache.insertScalar Nothing "int4" 23 1007 OidCache.empty
          names = HashSet.fromList [(Nothing, "int4"), (Nothing, "int8")]
      OidCache.selectUnknownNames names cache
        `shouldBe` HashSet.fromList [(Nothing, "int8")]

  describe "Semigroup" do
    it "right operand takes precedence for duplicate keys" do
      let cacheA = OidCache.insertScalar Nothing "int4" 23 1007 OidCache.empty
          cacheB = OidCache.insertScalar Nothing "int4" 99 999 OidCache.empty
          merged = cacheA <> cacheB
      OidCache.lookupScalar Nothing "int4" merged
        `shouldBe` Just 99
      OidCache.lookupArray Nothing "int4" merged
        `shouldBe` Just 999

    it "preserves entries from both sides when no conflict" do
      let cacheA = OidCache.insertScalar Nothing "int4" 23 1007 OidCache.empty
          cacheB = OidCache.insertScalar Nothing "int8" 20 1016 OidCache.empty
          merged = cacheA <> cacheB
      OidCache.lookupScalar Nothing "int4" merged
        `shouldBe` Just 23
      OidCache.lookupScalar Nothing "int8" merged
        `shouldBe` Just 20

    it "is associative" do
      let a = OidCache.insertScalar Nothing "t1" 1 2 OidCache.empty
          b = OidCache.insertScalar Nothing "t1" 3 4 (OidCache.insertScalar Nothing "t2" 5 6 OidCache.empty)
          c = OidCache.insertScalar Nothing "t2" 7 8 (OidCache.insertScalar Nothing "t3" 9 10 OidCache.empty)
      OidCache.toHashMap ((a <> b) <> c)
        `shouldBe` OidCache.toHashMap (a <> (b <> c))

  describe "Monoid" do
    it "mempty is identity for Semigroup" do
      let cache = OidCache.insertScalar Nothing "int4" 23 1007 OidCache.empty
      OidCache.toHashMap (cache <> mempty)
        `shouldBe` OidCache.toHashMap cache
      OidCache.toHashMap (mempty <> cache)
        `shouldBe` OidCache.toHashMap cache

    it "empty equals mempty" do
      OidCache.toHashMap OidCache.empty
        `shouldBe` OidCache.toHashMap mempty
