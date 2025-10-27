module Sharing.ByUnit.Session.CatchErrorSpec (spec) where

import Data.Either
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
  it "Leaves the session usable" \config -> do
    Scripts.onPreparableConnection config \connection -> do
      let tryStatement =
            Statement.preparable
              "select $1 :: int8"
              (Encoders.param (Encoders.nonNullable Encoders.int8))
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))

      result <-
        Connection.use connection do
          -- First successful query
          a <- Session.statement (1 :: Int64) tryStatement
          -- This should fail but connection should remain usable
          () <- catchError (Session.script "absurd") (const (pure ()))
          -- Second successful query
          b <- Session.statement (2 :: Int64) tryStatement
          pure (a, b)

      result `shouldBe` Right (1, 2)
