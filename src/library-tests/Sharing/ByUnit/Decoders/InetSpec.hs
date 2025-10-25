module Sharing.ByUnit.Decoders.InetSpec (spec) where

import Data.IP (AddrRange, IPv4, IPv6)
import Data.IP qualified as IP
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
  describe "INET Decoders" do
    it "decodes IPv4 address" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select '192.168.1.1/32'::inet"
                Encoders.noParams
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.inet)))
                True
        result <- Connection.use connection (Session.statement () statement)
        let expectedAddr = read "192.168.1.1" :: IPv4
            expectedRange = IP.makeAddrRange expectedAddr 32
        result `shouldBe` Right (IP.IPv4Range expectedRange)

    it "decodes IPv4 CIDR" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select '10.0.0.0/8'::inet"
                Encoders.noParams
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.inet)))
                True
        result <- Connection.use connection (Session.statement () statement)
        let expectedAddr = read "10.0.0.0" :: IPv4
            expectedRange = IP.makeAddrRange expectedAddr 8
        result `shouldBe` Right (IP.IPv4Range expectedRange)

    it "decodes IPv6 address" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select '::1/128'::inet"
                Encoders.noParams
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.inet)))
                True
        result <- Connection.use connection (Session.statement () statement)
        let expectedAddr = read "::1" :: IPv6
            expectedRange = IP.makeAddrRange expectedAddr 128
        result `shouldBe` Right (IP.IPv6Range expectedRange)

  describe "MACADDR Decoders" do
    it "decodes MAC address" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select '08:00:2b:01:02:03'::macaddr"
                Encoders.noParams
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.macaddr)))
                True
        result <- Connection.use connection (Session.statement () statement)
        result `shouldBe` Right (0x08, 0x00, 0x2b, 0x01, 0x02, 0x03)

    it "decodes another MAC address format" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select 'ff:ff:ff:ff:ff:ff'::macaddr"
                Encoders.noParams
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.macaddr)))
                True
        result <- Connection.use connection (Session.statement () statement)
        result `shouldBe` Right (0xff, 0xff, 0xff, 0xff, 0xff, 0xff)
