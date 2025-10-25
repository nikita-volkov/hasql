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
                Statement.Statement
                  "select (1::int8, true)"
                  mempty
                  ( Decoders.singleRow
                      ( Decoders.column
                          ( Decoders.nonNullable
                              ( Decoders.record
                                  ( (,)
                                      <$> Decoders.field (Decoders.nonNullable Decoders.int8)
                                      <*> Decoders.field (Decoders.nonNullable Decoders.bool)
                                  )
                              )
                          )
                      )
                  )
                  True
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right (1 :: Int64, True)

      it "decodes unnamed composites with different types" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.Statement
                  "select ('hello'::text, 123::int4)"
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
                  True
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right ("hello", 123 :: Int32)

      it "decodes unnamed composites with three fields" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.Statement
                  "select (42::int8, 'test'::text, 3.14 :: float8)"
                  mempty
                  ( Decoders.singleRow
                      ( Decoders.column
                          ( Decoders.nonNullable
                              ( Decoders.record
                                  ( (,,)
                                      <$> Decoders.field (Decoders.nonNullable Decoders.int8)
                                      <*> Decoders.field (Decoders.nonNullable Decoders.text)
                                      <*> Decoders.field (Decoders.nonNullable Decoders.float8)
                                  )
                              )
                          )
                      )
                  )
                  True
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right (42 :: Int64, "test", 3.14 :: Double)

    describe "Nested composites" do
      it "decodes nested unnamed composites from static SQL" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.Statement
                  "select ((1::int8, true), ('hello'::text, 3::int8))"
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
                                                    <$> Decoders.field (Decoders.nonNullable Decoders.int8)
                                                    <*> Decoders.field (Decoders.nonNullable Decoders.bool)
                                                )
                                            )
                                        )
                                      <*> Decoders.field
                                        ( Decoders.nonNullable
                                            ( Decoders.record
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
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right ((1 :: Int64, True), ("hello", 3 :: Int64))

      it "decodes deeply nested unnamed composites" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.Statement
                  "select ((row (99::int8), (true, 'test'::text)), 'outer'::text)"
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
                                                              (Decoders.field (Decoders.nonNullable Decoders.int8))
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
                  True
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right ((99 :: Int64, (True, "test")), "outer")

    describe "Arrays of composites" do
      it "decodes arrays of unnamed composites from static SQL" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.Statement
                  "select array[(1::int8, true), (2::int8, false), (3::int8, true)]"
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
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right [(1 :: Int64, True), (2, False), (3, True)]

      it "decodes 2D arrays of unnamed composites" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.Statement
                  "select array[array[(1::int4, 'a'::text), (2::int4, 'b'::text)], array[(3::int4, 'c'::text), (4::int4, 'd'::text)]]"
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
                  True
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right [[(1 :: Int32, "a"), (2, "b")], [(3, "c"), (4, "d")]]

      it "decodes arrays of nested unnamed composites" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.Statement
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
                  True
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right [((1, True), "x"), ((2, False), "y")]
