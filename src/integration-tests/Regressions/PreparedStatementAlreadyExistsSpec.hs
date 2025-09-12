module Regressions.PreparedStatementAlreadyExistsSpec (spec) where

import Data.Either
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec
import TestingKit.Testcontainers qualified as Testcontainers
import Prelude

spec :: Spec
spec = around Testcontainers.withConnection do
  it "Failing statements don't cause misses in updates of the prepared statement cache" \connection -> do
    -- Run an intentionally failing prepared statement to set the condition of the bug.
    result <- Connection.use connection do
      Session.statement
        ()
        ( Statement.Statement
            "select null"
            mempty
            (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
            True
        )
    -- Run a succeeding prepared statement to see if the cache is still in a good state.
    result <- Connection.use connection do
      Session.statement
        ()
        ( Statement.Statement
            "select 1"
            mempty
            (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
            True
        )
    -- If there is an error the cache got corrupted.
    case result of
      Right _ ->
        pure ()
      Left result ->
        expectationFailure ("Unexpected error: " <> show result)
