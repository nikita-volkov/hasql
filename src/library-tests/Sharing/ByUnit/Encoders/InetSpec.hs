module Sharing.ByUnit.Encoders.InetSpec (spec) where

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
  describe "INET Encoders" do
    it "encodes IPv4 address correctly" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select $1 = '192.168.1.1/32'::inet"
                (Encoders.param (Encoders.nonNullable Encoders.inet))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
                True
            testAddr = read "192.168.1.1" :: IPv4
            testRange = IP.makeAddrRange testAddr 32
        result <- Connection.use connection (Session.statement (IP.IPv4Range testRange) statement)
        result `shouldBe` Right True

    it "roundtrips IPv4 CIDR" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select $1"
                (Encoders.param (Encoders.nonNullable Encoders.inet))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.inet)))
                True
            testAddr = read "10.0.0.0" :: IPv4
            testRange = IP.makeAddrRange testAddr 8
        result <- Connection.use connection (Session.statement (IP.IPv4Range testRange) statement)
        result `shouldBe` Right (IP.IPv4Range testRange)

    it "roundtrips IPv6 address" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select $1"
                (Encoders.param (Encoders.nonNullable Encoders.inet))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.inet)))
                True
            testAddr = read "2001:db8::1" :: IPv6
            testRange = IP.makeAddrRange testAddr 128
        result <- Connection.use connection (Session.statement (IP.IPv6Range testRange) statement)
        result `shouldBe` Right (IP.IPv6Range testRange)

  describe "MACADDR Encoders" do
    it "encodes MAC address correctly" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select $1 = '08:00:2b:01:02:03'::macaddr"
                (Encoders.param (Encoders.nonNullable Encoders.macaddr))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
                True
            testMac = (0x08, 0x00, 0x2b, 0x01, 0x02, 0x03)
        result <- Connection.use connection (Session.statement testMac statement)
        result `shouldBe` Right True

    it "roundtrips MAC address" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let statement =
              Statement.Statement
                "select $1"
                (Encoders.param (Encoders.nonNullable Encoders.macaddr))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.macaddr)))
                True
            testMac = (0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff)
        result <- Connection.use connection (Session.statement testMac statement)
        result `shouldBe` Right testMac
