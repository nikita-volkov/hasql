module Sharing.ByUnit.Decoders.RenameSpec (spec) where

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
  describe "rename" do
    it "decodes a domain type using a renamed base codec" \config -> do
      domainName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          Session.statement ()
            $ Statement.preparable
              (mconcat ["create domain ", domainName, " as text"])
              mempty
              Decoders.noResult
          Session.statement ()
            $ Statement.preparable
              (mconcat ["select 'hello' :: ", domainName])
              mempty
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.rename Nothing domainName Decoders.text))))
        result `shouldBe` Right "hello"

    it "roundtrips a domain type using renamed encoder and decoder" \config -> do
      domainName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          Session.statement ()
            $ Statement.preparable
              (mconcat ["create domain ", domainName, " as text"])
              mempty
              Decoders.noResult
          Session.statement "world"
            $ Statement.preparable
              (mconcat ["select $1 :: ", domainName])
              (Encoders.param (Encoders.nonNullable (Encoders.rename Nothing domainName Encoders.text)))
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.rename Nothing domainName Decoders.text))))
        result `shouldBe` Right "world"

    it "decodes a schema-qualified domain type using a renamed base codec" \config -> do
      schemaName <- Scripts.generateSymname
      domainName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          Session.statement ()
            $ Statement.preparable
              (mconcat ["create schema ", schemaName])
              mempty
              Decoders.noResult
          Session.statement ()
            $ Statement.preparable
              (mconcat ["create domain ", schemaName, ".", domainName, " as int8"])
              mempty
              Decoders.noResult
          Session.statement ()
            $ Statement.preparable
              (mconcat ["select 42 :: ", schemaName, ".", domainName])
              mempty
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.rename (Just schemaName) domainName Decoders.int8))))
        result `shouldBe` Right (42 :: Int64)
