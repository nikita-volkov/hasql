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
    result1 <- Connection.use connection do
      Session.statement
        ()
        ( Statement.Statement
            "select null"
            mempty
            (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
            True
        )
    
    -- Debug: print the first error
    case result1 of
      Left err -> pure () -- Expected to fail
      Right _ -> expectationFailure "First statement should have failed"
    
    -- Run a succeeding prepared statement to see if the cache is still in a good state.
    result2 <- Connection.use connection do
      Session.statement
        ()
        ( Statement.Statement
            "select 1"
            mempty
            (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
            True
        )
    -- If there is an error the cache got corrupted.
    case result2 of
      Right _ ->
        pure ()
      Left err ->
        expectationFailure ("Unexpected error: " <> show err)
