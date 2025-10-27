module Sharing.ByUnit.Decoders.RecordSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Test.Hspec
import Prelude

spec :: SpecWith (Text, Word16)
spec = do
  describe "Unnamed Composite Decoders" do
    describe "Simple composites" do
      it "decodes a simple unnamed composite from static SQL" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.preparable
                  "select (1, true)"
                  mempty
                  ( Decoders.singleRow
                      ( Decoders.column
                          ( Decoders.nonNullable
                              ( Decoders.record
                                  ( (,)
                                      <$> Decoders.field (Decoders.nonNullable Decoders.int4)
                                      <*> Decoders.field (Decoders.nonNullable Decoders.bool)
                                  )
                              )
                          )
                      )
                  )
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right (1, True)

      it "decodes unnamed composites with different types" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.preparable
                  "select (text 'hello', 123)"
                  mempty
                  ( Decoders.singleRow
                      ( Decoders.column
                          ( Decoders.nonNullable
                              ( Decoders.record
                                  ( (,)
                                      <$> Decoders.field (Decoders.nonNullable Decoders.text)
                                      <*> Decoders.field (Decoders.nonNullable Decoders.int4)
                                  )
                              )
                          )
                      )
                  )
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right ("hello", 123 :: Int32)

      it "decodes unnamed composites with three fields" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.preparable
                  "select (42, text 'test', 3.14 :: float8)"
                  mempty
                  ( Decoders.singleRow
                      ( Decoders.column
                          ( Decoders.nonNullable
                              ( Decoders.record
                                  ( (,,)
                                      <$> Decoders.field (Decoders.nonNullable Decoders.int4)
                                      <*> Decoders.field (Decoders.nonNullable Decoders.text)
                                      <*> Decoders.field (Decoders.nonNullable Decoders.float8)
                                  )
                              )
                          )
                      )
                  )
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right (42, "test", 3.14 :: Double)

    describe "Nested composites" do
      it "decodes nested unnamed composites from static SQL" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.preparable
                  "select ((1, true), (text 'hello', 3))"
                  mempty
                  ( Decoders.singleRow
                      ( Decoders.column
                          ( Decoders.nonNullable
                              ( Decoders.record
                                  ( (,)
                                      <$> Decoders.field
                                        ( Decoders.nonNullable
                                            ( Decoders.record
                                                ( (,)
                                                    <$> Decoders.field (Decoders.nonNullable Decoders.int4)
                                                    <*> Decoders.field (Decoders.nonNullable Decoders.bool)
                                                )
                                            )
                                        )
                                      <*> Decoders.field
                                        ( Decoders.nonNullable
                                            ( Decoders.record
                                                ( (,)
                                                    <$> Decoders.field (Decoders.nonNullable Decoders.text)
                                                    <*> Decoders.field (Decoders.nonNullable Decoders.int4)
                                                )
                                            )
                                        )
                                  )
                              )
                          )
                      )
                  )
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right ((1, True), ("hello", 3))

      it "decodes deeply nested unnamed composites" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.preparable
                  "select ((row (99), (true, text 'test')), text 'outer')"
                  mempty
                  ( Decoders.singleRow
                      ( Decoders.column
                          ( Decoders.nonNullable
                              ( Decoders.record
                                  ( (,)
                                      <$> Decoders.field
                                        ( Decoders.nonNullable
                                            ( Decoders.record
                                                ( (,)
                                                    <$> Decoders.field
                                                      ( Decoders.nonNullable
                                                          ( Decoders.record
                                                              (Decoders.field (Decoders.nonNullable Decoders.int4))
                                                          )
                                                      )
                                                    <*> Decoders.field
                                                      ( Decoders.nonNullable
                                                          ( Decoders.record
                                                              ( (,)
                                                                  <$> Decoders.field (Decoders.nonNullable Decoders.bool)
                                                                  <*> Decoders.field (Decoders.nonNullable Decoders.text)
                                                              )
                                                          )
                                                      )
                                                )
                                            )
                                        )
                                      <*> Decoders.field (Decoders.nonNullable Decoders.text)
                                  )
                              )
                          )
                      )
                  )
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right ((99, (True, "test")), "outer")

    describe "Arrays of composites" do
      it "decodes arrays of unnamed composites from static SQL" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.preparable
                  "select array[(1, true), (2, false), (3, true)]"
                  mempty
                  ( Decoders.singleRow
                      ( Decoders.column
                          ( Decoders.nonNullable
                              ( Decoders.array
                                  ( Decoders.dimension
                                      replicateM
                                      ( Decoders.element
                                          ( Decoders.nonNullable
                                              ( Decoders.record
                                                  ( (,)
                                                      <$> Decoders.field (Decoders.nonNullable Decoders.int4)
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
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right [(1, True), (2, False), (3, True)]

      it "decodes 2D arrays of unnamed composites" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.preparable
                  "select array[array[(1, text 'a'), (2, text 'b')], array[(3, text 'c'), (4, text 'd')]]"
                  mempty
                  ( Decoders.singleRow
                      ( Decoders.column
                          ( Decoders.nonNullable
                              ( Decoders.array
                                  ( Decoders.dimension
                                      replicateM
                                      ( Decoders.dimension
                                          replicateM
                                          ( Decoders.element
                                              ( Decoders.nonNullable
                                                  ( Decoders.record
                                                      ( (,)
                                                          <$> Decoders.field (Decoders.nonNullable Decoders.int4)
                                                          <*> Decoders.field (Decoders.nonNullable Decoders.text)
                                                      )
                                                  )
                                              )
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right [[(1 :: Int32, "a"), (2, "b")], [(3, "c"), (4, "d")]]

      it "decodes arrays of nested unnamed composites" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.preparable
                  "select array[((1, true), text 'x'), ((2, false), text 'y')]"
                  mempty
                  ( Decoders.singleRow
                      ( Decoders.column
                          ( Decoders.nonNullable
                              ( Decoders.array
                                  ( Decoders.dimension
                                      replicateM
                                      ( Decoders.element
                                          ( Decoders.nonNullable
                                              ( Decoders.record
                                                  ( (,)
                                                      <$> Decoders.field
                                                        ( Decoders.nonNullable
                                                            ( Decoders.record
                                                                ( (,)
                                                                    <$> Decoders.field (Decoders.nonNullable Decoders.int4)
                                                                    <*> Decoders.field (Decoders.nonNullable Decoders.bool)
                                                                )
                                                            )
                                                        )
                                                      <*> Decoders.field (Decoders.nonNullable Decoders.text)
                                                  )
                                              )
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right [((1, True), "x"), ((2, False), "y")]
