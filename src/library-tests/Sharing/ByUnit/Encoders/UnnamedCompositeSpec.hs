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
  describe "Simple composites" do
    it "encodes a simple named composite and compares with static value" \config -> do
      typeName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create composite type
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["create type ", typeName, " as (x int8, y bool)"]))
              mempty
              Decoders.noResult
              True
          -- Test encoding by comparing with static value
          Session.statement (42 :: Int64, True)
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select ($1 :: ", typeName, ") = (42, true) :: ", typeName]))
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
        result `shouldBe` Right True

    it "Fails to encode a simple unnamed composite and compares with static value" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select $1 = row (42, true)"
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
        result `shouldNotBe` Right True

  describe "Nested composites" do
    it "Fails to encode nested unnamed composites" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select $1 = row (row (1, true), row ('hello', 3))"
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
        result `shouldNotBe` Right True

    it "Fails to roundtrip nested unnamed composites" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select $1 :: record"
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
        result `shouldNotBe` Right ((1 :: Int64, True), ("hello", 3 :: Int64))

  describe "Arrays of composites" do
    it "Fails to encode arrays of unnamed composites" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select $1 = array[row (1, true), row (2, false), row (3, true)]"
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
        result `shouldNotBe` Right True
