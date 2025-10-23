module Sharing.ByUnit.Encoders.UnnamedCompositeSpec (spec) where

import Data.Text.Encoding (encodeUtf8)
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Test.Hspec
import Prelude

spec :: SpecWith (Text, Word16)
spec = do
  describe "Unnamed Composite Encoders" do
    describe "Simple composites" do
      it "encodes a simple unnamed composite and compares with static value" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.Statement
                  "select $1 = (42, true)"
                  ( Encoders.param
                      ( Encoders.nonNullable
                          ( Encoders.unnamedComposite
                              ( divide
                                  (\(a, b) -> (a, b))
                                  (Encoders.field (Encoders.nonNullable Encoders.int8))
                                  (Encoders.field (Encoders.nonNullable Encoders.bool))
                              )
                          )
                      )
                  )
                  (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
                  True
          result <- Connection.use connection (Session.statement (42 :: Int64, True) statement)
          result `shouldBe` Right True

      it "encodes and roundtrips a simple unnamed composite" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.Statement
                  "select $1"
                  ( Encoders.param
                      ( Encoders.nonNullable
                          ( Encoders.unnamedComposite
                              ( divide
                                  (\(a, b) -> (a, b))
                                  (Encoders.field (Encoders.nonNullable Encoders.int8))
                                  (Encoders.field (Encoders.nonNullable Encoders.bool))
                              )
                          )
                      )
                  )
                  ( Decoders.singleRow
                      ( Decoders.column
                          ( Decoders.nonNullable
                              ( Decoders.unnamedComposite
                                  ( (,)
                                      <$> Decoders.field (Decoders.nonNullable Decoders.int8)
                                      <*> Decoders.field (Decoders.nonNullable Decoders.bool)
                                  )
                              )
                          )
                      )
                  )
                  True
          result <- Connection.use connection (Session.statement (42 :: Int64, True) statement)
          result `shouldBe` Right (42 :: Int64, True)

      it "encodes unnamed composites with different types" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.Statement
                  "select $1 = ('hello', 123)"
                  ( Encoders.param
                      ( Encoders.nonNullable
                          ( Encoders.unnamedComposite
                              ( divide
                                  (\(a, b) -> (a, b))
                                  (Encoders.field (Encoders.nonNullable Encoders.text))
                                  (Encoders.field (Encoders.nonNullable Encoders.int4))
                              )
                          )
                      )
                  )
                  (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
                  True
          result <- Connection.use connection (Session.statement ("hello", 123 :: Int32) statement)
          result `shouldBe` Right True

    describe "Nested composites" do
      it "encodes nested unnamed composites" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.Statement
                  "select $1 = ((1, true), ('hello', 3))"
                  ( Encoders.param
                      ( Encoders.nonNullable
                          ( Encoders.unnamedComposite
                              ( divide
                                  (\(inner1, inner2) -> (inner1, inner2))
                                  ( Encoders.field
                                      ( Encoders.nonNullable
                                          ( Encoders.unnamedComposite
                                              ( divide
                                                  (\(a, b) -> (a, b))
                                                  (Encoders.field (Encoders.nonNullable Encoders.int8))
                                                  (Encoders.field (Encoders.nonNullable Encoders.bool))
                                              )
                                          )
                                      )
                                  )
                                  ( Encoders.field
                                      ( Encoders.nonNullable
                                          ( Encoders.unnamedComposite
                                              ( divide
                                                  (\(c, d) -> (c, d))
                                                  (Encoders.field (Encoders.nonNullable Encoders.text))
                                                  (Encoders.field (Encoders.nonNullable Encoders.int8))
                                              )
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
                  (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
                  True
          result <- Connection.use connection (Session.statement ((1 :: Int64, True), ("hello", 3 :: Int64)) statement)
          result `shouldBe` Right True

      it "roundtrips nested unnamed composites" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.Statement
                  "select $1"
                  ( Encoders.param
                      ( Encoders.nonNullable
                          ( Encoders.unnamedComposite
                              ( divide
                                  (\(inner1, inner2) -> (inner1, inner2))
                                  ( Encoders.field
                                      ( Encoders.nonNullable
                                          ( Encoders.unnamedComposite
                                              ( divide
                                                  (\(a, b) -> (a, b))
                                                  (Encoders.field (Encoders.nonNullable Encoders.int8))
                                                  (Encoders.field (Encoders.nonNullable Encoders.bool))
                                              )
                                          )
                                      )
                                  )
                                  ( Encoders.field
                                      ( Encoders.nonNullable
                                          ( Encoders.unnamedComposite
                                              ( divide
                                                  (\(c, d) -> (c, d))
                                                  (Encoders.field (Encoders.nonNullable Encoders.text))
                                                  (Encoders.field (Encoders.nonNullable Encoders.int8))
                                              )
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
                  ( Decoders.singleRow
                      ( Decoders.column
                          ( Decoders.nonNullable
                              ( Decoders.unnamedComposite
                                  ( (,)
                                      <$> Decoders.field
                                        ( Decoders.nonNullable
                                            ( Decoders.unnamedComposite
                                                ( (,)
                                                    <$> Decoders.field (Decoders.nonNullable Decoders.int8)
                                                    <*> Decoders.field (Decoders.nonNullable Decoders.bool)
                                                )
                                            )
                                        )
                                      <*> Decoders.field
                                        ( Decoders.nonNullable
                                            ( Decoders.unnamedComposite
                                                ( (,)
                                                    <$> Decoders.field (Decoders.nonNullable Decoders.text)
                                                    <*> Decoders.field (Decoders.nonNullable Decoders.int8)
                                                )
                                            )
                                        )
                                  )
                              )
                          )
                      )
                  )
                  True
          result <- Connection.use connection (Session.statement ((1 :: Int64, True), ("hello", 3 :: Int64)) statement)
          result `shouldBe` Right ((1 :: Int64, True), ("hello", 3 :: Int64))

    describe "Arrays of composites" do
      it "encodes arrays of unnamed composites" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.Statement
                  "select $1 = array[(1, true), (2, false), (3, true)]"
                  ( Encoders.param
                      ( Encoders.nonNullable
                          ( Encoders.array
                              ( Encoders.dimension
                                  foldl'
                                  ( Encoders.element
                                      ( Encoders.nonNullable
                                          ( Encoders.unnamedComposite
                                              ( divide
                                                  (\(a, b) -> (a, b))
                                                  (Encoders.field (Encoders.nonNullable Encoders.int8))
                                                  (Encoders.field (Encoders.nonNullable Encoders.bool))
                                              )
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
                  (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
                  True
          result <- Connection.use connection (Session.statement [(1 :: Int64, True), (2, False), (3, True)] statement)
          result `shouldBe` Right True

      it "roundtrips arrays of unnamed composites" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.Statement
                  "select $1"
                  ( Encoders.param
                      ( Encoders.nonNullable
                          ( Encoders.array
                              ( Encoders.dimension
                                  foldl'
                                  ( Encoders.element
                                      ( Encoders.nonNullable
                                          ( Encoders.unnamedComposite
                                              ( divide
                                                  (\(a, b) -> (a, b))
                                                  (Encoders.field (Encoders.nonNullable Encoders.int8))
                                                  (Encoders.field (Encoders.nonNullable Encoders.bool))
                                              )
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
                  ( Decoders.singleRow
                      ( Decoders.column
                          ( Decoders.nonNullable
                              ( Decoders.array
                                  ( Decoders.dimension
                                      replicateM
                                      ( Decoders.element
                                          ( Decoders.nonNullable
                                              ( Decoders.unnamedComposite
                                                  ( (,)
                                                      <$> Decoders.field (Decoders.nonNullable Decoders.int8)
                                                      <*> Decoders.field (Decoders.nonNullable Decoders.bool)
                                                  )
                                              )
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
                  True
          result <- Connection.use connection (Session.statement [(1 :: Int64, True), (2, False), (3, True)] statement)
          result `shouldBe` Right [(1 :: Int64, True), (2, False), (3, True)]
