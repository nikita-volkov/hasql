module Sharing.ByUnit.Decoders.Composite.OidMismatchSpec (spec) where

import Data.Text.Encoding (encodeUtf8)
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Errors qualified as Errors
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Test.Hspec
import Prelude

spec :: SpecWith (Text, Word16)
spec = do
  describe "Composite field OID mismatch detection" do
    describe "Decoder field type mismatch" do
      it "detects when decoder expects int4 but actual field is int8" \config -> do
        typeName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create composite type with int8 field
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", typeName, " as (x int8)"]))
                mempty
                Decoders.noResult
                True
            -- Try to decode with int4 decoder
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select row(42) :: ", typeName]))
                mempty
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            ( Decoders.composite
                                Nothing
                                typeName
                                -- Using int4 decoder for int8 field - should fail
                                (Decoders.field (Decoders.nonNullable Decoders.int4))
                            )
                        )
                    )
                )
                True
          -- The error should indicate a decoding failure due to type mismatch
          case result of
            Left (Errors.StatementSessionError _ _ _ _ _ (Errors.CellStatementError _ _ (Errors.DeserializationCellError msg))) -> do
              -- PostgreSQL binary decoder should detect the OID mismatch
              toList msg `shouldContain` "int"
            Left err ->
              expectationFailure ("Unexpected type of error: " <> show err)
            Right _ ->
              expectationFailure "Expected an error but got success"

      it "detects when decoder expects int8 but actual field is int4" \config -> do
        typeName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create composite type with int4 field
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", typeName, " as (x int4)"]))
                mempty
                Decoders.noResult
                True
            -- Try to decode with int8 decoder
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select row(42) :: ", typeName]))
                mempty
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            ( Decoders.composite
                                Nothing
                                typeName
                                -- Using int8 decoder for int4 field - should fail
                                (Decoders.field (Decoders.nonNullable Decoders.int8))
                            )
                        )
                    )
                )
                True
          -- The error should indicate a decoding failure due to type mismatch
          case result of
            Left (Errors.StatementSessionError _ _ _ _ _ (Errors.CellStatementError _ _ (Errors.DeserializationCellError msg))) -> do
              -- PostgreSQL binary decoder should detect the OID mismatch
              toList msg `shouldContain` "int"
            Left err ->
              expectationFailure ("Unexpected type of error: " <> show err)
            Right _ ->
              expectationFailure "Expected an error but got success"

      it "detects when decoder expects text but actual field is int8" \config -> do
        typeName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create composite type with int8 field
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", typeName, " as (x int8)"]))
                mempty
                Decoders.noResult
                True
            -- Try to decode with text decoder
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select row(42) :: ", typeName]))
                mempty
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            ( Decoders.composite
                                Nothing
                                typeName
                                -- Using text decoder for int8 field - should fail
                                (Decoders.field (Decoders.nonNullable Decoders.text))
                            )
                        )
                    )
                )
                True
          -- The error should indicate a decoding failure due to type mismatch
          case result of
            Left (Errors.StatementSessionError _ _ _ _ _ (Errors.CellStatementError _ _ (Errors.DeserializationCellError _msg))) -> do
              -- PostgreSQL binary decoder should detect the type mismatch
              pure ()
            Left err ->
              expectationFailure ("Unexpected type of error: " <> show err)
            Right _ ->
              expectationFailure "Expected an error but got success"

    describe "Multiple fields with mismatches" do
      it "detects mismatch in second field" \config -> do
        typeName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create composite type with int8, int4 fields
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["create type ", typeName, " as (a int8, b int4)"]))
                mempty
                Decoders.noResult
                True
            -- Try to decode with correct first field but wrong second field
            Session.statement ()
              $ Statement.Statement
                (encodeUtf8 (mconcat ["select row(1, 2) :: ", typeName]))
                mempty
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            ( Decoders.composite
                                Nothing
                                typeName
                                ( (,)
                                    <$> Decoders.field (Decoders.nonNullable Decoders.int8)
                                    -- Using int8 decoder for int4 field - should fail
                                    <*> Decoders.field (Decoders.nonNullable Decoders.int8)
                                )
                            )
                        )
                    )
                )
                True
          -- The error should indicate a decoding failure
          case result of
            Left (Errors.StatementSessionError _ _ _ _ _ (Errors.CellStatementError _ _ (Errors.DeserializationCellError _msg))) -> do
              pure ()
            Left err ->
              expectationFailure ("Unexpected type of error: " <> show err)
            Right _ ->
              expectationFailure "Expected an error but got success"
