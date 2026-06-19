module Sharing.ByUnit.Encoders.CitextSpec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Test.Hspec
import Prelude

spec :: SpecWith Scripts.ScopeParams
spec = do
  describe "Citext Encoders" do
    it "encodes a citext value and compares with static value" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          catchError
            ( Session.statement ()
                $ Statement.unpreparable
                  "CREATE EXTENSION IF NOT EXISTS citext"
                  Encoders.noParams
                  Decoders.noResult
            )
            (const (pure ()))
          Session.statement "hello"
            $ Statement.preparable
              "select $1 = 'hello'"
              (Encoders.param (Encoders.nonNullable Encoders.citext))
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        result `shouldBe` Right True

    it "encodes a citext value with case-insensitive comparison" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          catchError
            ( Session.statement ()
                $ Statement.unpreparable
                  "CREATE EXTENSION IF NOT EXISTS citext"
                  Encoders.noParams
                  Decoders.noResult
            )
            (const (pure ()))
          Session.statement "Hello"
            $ Statement.preparable
              "select $1 = 'hello'"
              (Encoders.param (Encoders.nonNullable Encoders.citext))
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        result `shouldBe` Right True

    it "roundtrips a citext value" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          catchError
            ( Session.statement ()
                $ Statement.unpreparable
                  "CREATE EXTENSION IF NOT EXISTS citext"
                  Encoders.noParams
                  Decoders.noResult
            )
            (const (pure ()))
          Session.statement "Hello World"
            $ Statement.preparable
              "select $1"
              (Encoders.param (Encoders.nonNullable Encoders.citext))
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.citext)))
        result `shouldBe` Right "Hello World"
