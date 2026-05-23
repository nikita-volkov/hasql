module Sharing.ByUnit.Encoders.RenameSpec (spec) where

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
    it "encodes a value using a renamed base codec targeting a domain type" \config -> do
      domainName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          Session.statement ()
            $ Statement.preparable
              (mconcat ["create domain ", domainName, " as text"])
              mempty
              Decoders.noResult
          Session.statement "hello"
            $ Statement.preparable
              (mconcat ["select ($1 :: ", domainName, ") = 'hello' :: ", domainName])
              (Encoders.param (Encoders.nonNullable (Encoders.rename Nothing domainName Encoders.text)))
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        result `shouldBe` Right True

    it "roundtrips a domain type using renamed encoder and decoder" \config -> do
      domainName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          Session.statement ()
            $ Statement.preparable
              (mconcat ["create domain ", domainName, " as int8"])
              mempty
              Decoders.noResult
          Session.statement (42 :: Int64)
            $ Statement.preparable
              (mconcat ["select $1 :: ", domainName])
              (Encoders.param (Encoders.nonNullable (Encoders.rename Nothing domainName Encoders.int8)))
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.rename Nothing domainName Decoders.int8))))
        result `shouldBe` Right (42 :: Int64)

    it "encodes using a schema-qualified renamed codec" \config -> do
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
              (mconcat ["create domain ", schemaName, ".", domainName, " as text"])
              mempty
              Decoders.noResult
          Session.statement "test"
            $ Statement.preparable
              (mconcat ["select ($1 :: ", schemaName, ".", domainName, ") = 'test' :: ", schemaName, ".", domainName])
              (Encoders.param (Encoders.nonNullable (Encoders.rename (Just schemaName) domainName Encoders.text)))
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
        result `shouldBe` Right True
