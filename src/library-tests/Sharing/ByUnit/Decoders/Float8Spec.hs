module Sharing.ByUnit.Decoders.Float8Spec (spec) where

import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Test.Hspec
import Prelude

spec :: SpecWith (Text, Word16)
spec = do
  it "decodes static value properly" \config -> do
    Scripts.onPreparableConnection config \connection -> do
      let statement =
            Statement.Statement
              "select 3.14 :: float8"
              mempty
              ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          Decoders.float8
                      )
                  )
              )
              True
      result <- Connection.use connection (Session.statement () statement)
      result `shouldBe` Right 3.14
