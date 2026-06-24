module Sharing.ByUnit.Decoders.CitextSpec (spec) where

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
  describe "Citext Decoders" do
    it "decodes a citext value" \config -> do
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
          Session.statement ()
            $ Statement.preparable
              "select 'Hello World'::citext"
              mempty
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.citext)))
        result `shouldBe` Right "Hello World"

    it "decodes a citext value preserving case" \config -> do
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
          Session.statement ()
            $ Statement.preparable
              "select 'HeLLo WoRLd'::citext"
              mempty
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.citext)))
        result `shouldBe` Right "HeLLo WoRLd"

    it "decodes a nullable citext value" \config -> do
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
          Session.statement ()
            $ Statement.preparable
              "select null::citext"
              mempty
              (Decoders.singleRow (Decoders.column (Decoders.nullable Decoders.citext)))
        result `shouldBe` Right (Nothing :: Maybe Text)

    it "decodes citext case-insensitive comparison in SQL" \config -> do
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
          Session.statement ()
            $ Statement.preparable
              "select 'hello'::citext = 'HELLO'::citext"
              mempty
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        result `shouldBe` Right True
