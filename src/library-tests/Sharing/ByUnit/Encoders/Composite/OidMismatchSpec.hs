module Sharing.ByUnit.Encoders.Composite.OidMismatchSpec (spec) where

import Data.Text.Encoding (encodeUtf8)
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Errors qualified as Errors
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Test.Hspec
import Prelude

spec :: SpecWith (Text, Word16)
spec = do
  describe "Composite field OID mismatch detection" do
    describe "Encoder field type mismatch" do
      it "detects when encoder uses int4 but actual field is int8" \config -> do
        typeName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create composite type with int8 field
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", typeName, " as (x int8)"]))
                mempty
                Decoders.noResult
            -- Try to encode with int4 encoder to int8 field
            Session.statement (42 :: Int32)
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select $1 :: ", typeName]))
                ( Encoders.param
                    ( Encoders.nonNullable
                        ( Encoders.composite
                            Nothing
                            typeName
                            -- Using int4 encoder for int8 field - should fail
                            (Encoders.field (Encoders.nonNullable Encoders.int4))
                        )
                    )
                )
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            (Decoders.composite Nothing typeName (Decoders.field (Decoders.nonNullable Decoders.int8)))
                        )
                    )
                )
          -- The error should indicate a type mismatch from the server
          case result of
            Left (Errors.StatementSessionError _ _ _ _ _ (Errors.ExecutionStatementError (Errors.ExecutionError code _msg _detail _hint _pos))) -> do
              -- PostgreSQL should reject the mismatched types
              -- Error code 42804 is "datatype_mismatch"
              code `shouldSatisfy` (\c -> c == "42804" || c == "42P01" || c == "22P02")
            Left err ->
              expectationFailure ("Unexpected type of error: " <> show err)
            Right _ ->
              expectationFailure "Expected an error but got success"

      it "detects when encoder uses int8 but actual field is int4" \config -> do
        typeName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create composite type with int4 field
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", typeName, " as (x int4)"]))
                mempty
                Decoders.noResult
            -- Try to encode with int8 encoder to int4 field
            Session.statement (42 :: Int64)
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select $1 :: ", typeName]))
                ( Encoders.param
                    ( Encoders.nonNullable
                        ( Encoders.composite
                            Nothing
                            typeName
                            -- Using int8 encoder for int4 field - should fail
                            (Encoders.field (Encoders.nonNullable Encoders.int8))
                        )
                    )
                )
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            (Decoders.composite Nothing typeName (Decoders.field (Decoders.nonNullable Decoders.int4)))
                        )
                    )
                )
          -- The error should indicate a type mismatch from the server
          case result of
            Left (Errors.StatementSessionError _ _ _ _ _ (Errors.ExecutionStatementError (Errors.ExecutionError code _msg _detail _hint _pos))) -> do
              -- PostgreSQL should reject the mismatched types
              -- Error code 42804 is "datatype_mismatch"
              code `shouldSatisfy` (\c -> c == "42804" || c == "42P01" || c == "22P02")
            Left err ->
              expectationFailure ("Unexpected type of error: " <> show err)
            Right _ ->
              expectationFailure "Expected an error but got success"

      it "detects when encoder uses text but actual field is int8" \config -> do
        typeName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            -- Create composite type with int8 field
            Session.statement ()
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", typeName, " as (x int8)"]))
                mempty
                Decoders.noResult
            -- Try to encode with text encoder to int8 field
            Session.statement ("hello" :: Text)
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select $1 :: ", typeName]))
                ( Encoders.param
                    ( Encoders.nonNullable
                        ( Encoders.composite
                            Nothing
                            typeName
                            -- Using text encoder for int8 field - should fail
                            (Encoders.field (Encoders.nonNullable Encoders.text))
                        )
                    )
                )
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            (Decoders.composite Nothing typeName (Decoders.field (Decoders.nonNullable Decoders.int8)))
                        )
                    )
                )
          -- The error should indicate a type mismatch from the server
          case result of
            Left (Errors.StatementSessionError _ _ _ _ _ (Errors.ExecutionStatementError (Errors.ExecutionError code _msg _detail _hint _pos))) -> do
              -- PostgreSQL should reject the mismatched types
              code `shouldSatisfy` (\c -> c == "42804" || c == "42P01" || c == "22P02")
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
              $ Statement.preparable
                (encodeUtf8 (mconcat ["create type ", typeName, " as (a int8, b int4)"]))
                mempty
                Decoders.noResult
            -- Try to encode with correct first field but wrong second field
            Session.statement (1 :: Int64, 2 :: Int64)
              $ Statement.preparable
                (encodeUtf8 (mconcat ["select $1 :: ", typeName]))
                ( Encoders.param
                    ( Encoders.nonNullable
                        ( Encoders.composite
                            Nothing
                            typeName
                            ( divide
                                (\(a, b) -> (a, b))
                                (Encoders.field (Encoders.nonNullable Encoders.int8))
                                -- Using int8 encoder for int4 field - should fail
                                (Encoders.field (Encoders.nonNullable Encoders.int8))
                            )
                        )
                    )
                )
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            ( Decoders.composite
                                Nothing
                                typeName
                                ( (,)
                                    <$> Decoders.field (Decoders.nonNullable Decoders.int8)
                                    <*> Decoders.field (Decoders.nonNullable Decoders.int4)
                                )
                            )
                        )
                    )
                )
          -- The error should indicate a type mismatch from the server
          case result of
            Left (Errors.StatementSessionError _ _ _ _ _ (Errors.ExecutionStatementError (Errors.ExecutionError code _msg _detail _hint _pos))) -> do
              -- PostgreSQL should reject the mismatched types
              code `shouldSatisfy` (\c -> c == "42804" || c == "42P01" || c == "22P02")
            Left err ->
              expectationFailure ("Unexpected type of error: " <> show err)
            Right _ ->
              expectationFailure "Expected an error but got success"
