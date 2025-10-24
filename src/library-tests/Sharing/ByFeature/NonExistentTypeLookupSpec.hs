module Sharing.ByFeature.NonExistentTypeLookupSpec (spec) where

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
  describe "Non-existent type lookups" do
    describe "Enum types" do
      it "detects attempts to encode non-existent enum types" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            Session.statement "test_value"
              $ Statement.Statement
                "select $1::this_enum_does_not_exist_in_db"
                (Encoders.param (Encoders.nonNullable (Encoders.enum Nothing "this_enum_does_not_exist_in_db" id)))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
                True
          
          -- The statement should fail when trying to use a non-existent type
          -- This test documents the current behavior
          case result of
            Left err -> do
              -- We expect some kind of error
              err `shouldSatisfy` \case
                Errors.StatementSessionError {} -> True
                _ -> False
            Right _ -> 
              expectationFailure "Expected error when using non-existent enum type, but statement succeeded"

      it "detects attempts to decode non-existent enum types" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            Session.statement ()
              $ Statement.Statement
                "select 'value'::text"
                mempty
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable (Decoders.enum Nothing "nonexistent_enum_type" (Just . id)))))
                True
          
          -- The statement should fail when trying to decode with a non-existent type
          case result of
            Left err -> do
              -- We expect some kind of error (type mismatch or similar)
              err `shouldSatisfy` \case
                Errors.StatementSessionError {} -> True
                _ -> False
            Right _ ->
              expectationFailure "Expected error when decoding with non-existent enum type, but statement succeeded"

    describe "Composite types" do
      it "detects attempts to encode non-existent composite types" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            Session.statement (42 :: Int64, "test")
              $ Statement.Statement
                "select $1::nonexistent_composite_type"
                ( Encoders.param
                    ( Encoders.nonNullable
                        ( Encoders.composite
                            Nothing
                            "nonexistent_composite_type"
                            ( divide
                                (\(a, b) -> (a, b))
                                (Encoders.field (Encoders.nonNullable Encoders.int8))
                                (Encoders.field (Encoders.nonNullable Encoders.text))
                            )
                        )
                    )
                )
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
                True
          
          -- The statement should fail when trying to use a non-existent type
          case result of
            Left err -> do
              err `shouldSatisfy` \case
                Errors.StatementSessionError {} -> True
                _ -> False
            Right _ ->
              expectationFailure "Expected error when using non-existent composite type, but statement succeeded"

      it "detects attempts to decode non-existent composite types" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            Session.statement ()
              $ Statement.Statement
                "select row(42, 'test')"
                mempty
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            ( Decoders.composite
                                Nothing
                                "nonexistent_composite_type"
                                ( (,)
                                    <$> Decoders.field (Decoders.nonNullable Decoders.int8)
                                    <*> Decoders.field (Decoders.nonNullable Decoders.text)
                                )
                            )
                        )
                    )
                )
                True
          
          -- The statement should fail when trying to decode with a non-existent type
          case result of
            Left err -> do
              err `shouldSatisfy` \case
                Errors.StatementSessionError {} -> True
                _ -> False
            Right _ ->
              expectationFailure "Expected error when decoding with non-existent composite type, but statement succeeded"

    describe "Array types with non-existent element types" do
      it "detects attempts to encode arrays of non-existent enum types" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            Session.statement ["val1", "val2"]
              $ Statement.Statement
                "select $1::nonexistent_array_enum[]"
                ( Encoders.param
                    ( Encoders.nonNullable
                        ( Encoders.array
                            ( Encoders.dimension
                                foldl'
                                (Encoders.element (Encoders.nonNullable (Encoders.enum Nothing "nonexistent_array_enum" id)))
                            )
                        )
                    )
                )
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
                True
          
          -- The statement should fail when trying to use a non-existent type
          case result of
            Left err -> do
              err `shouldSatisfy` \case
                Errors.StatementSessionError {} -> True
                _ -> False
            Right _ ->
              expectationFailure "Expected error when using array of non-existent enum type, but statement succeeded"

      it "detects attempts to decode arrays of non-existent enum types" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            Session.statement ()
              $ Statement.Statement
                "select array['a', 'b']::text[]"
                mempty
                ( Decoders.singleRow
                    ( Decoders.column
                        ( Decoders.nonNullable
                            ( Decoders.array
                                ( Decoders.dimension
                                    replicateM
                                    (Decoders.element (Decoders.nonNullable (Decoders.enum Nothing "nonexistent_array_enum" (Just . id))))
                                )
                            )
                        )
                    )
                )
                True
          
          -- The statement should fail when trying to decode with a non-existent type
          case result of
            Left err -> do
              err `shouldSatisfy` \case
                Errors.StatementSessionError {} -> True
                _ -> False
            Right _ ->
              expectationFailure "Expected error when decoding array of non-existent enum type, but statement succeeded"

    describe "Namespaced types" do
      it "detects attempts to use non-existent type in non-existent schema" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            Session.statement "test"
              $ Statement.Statement
                "select $1::nonexistent_schema.nonexistent_type"
                (Encoders.param (Encoders.nonNullable (Encoders.enum (Just "nonexistent_schema") "nonexistent_type" id)))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
                True
          
          -- The statement should fail when trying to use a non-existent schema.type
          case result of
            Left err -> do
              err `shouldSatisfy` \case
                Errors.StatementSessionError {} -> True
                _ -> False
            Right _ ->
              expectationFailure "Expected error when using non-existent schema.type, but statement succeeded"

      it "detects attempts to use non-existent type in existing schema" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          result <- Connection.use connection do
            Session.statement "test"
              $ Statement.Statement
                "select $1::public.this_type_does_not_exist"
                (Encoders.param (Encoders.nonNullable (Encoders.enum (Just "public") "this_type_does_not_exist" id)))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
                True
          
          -- The statement should fail when trying to use a non-existent type in existing schema
          case result of
            Left err -> do
              err `shouldSatisfy` \case
                Errors.StatementSessionError {} -> True
                _ -> False
            Right _ ->
              expectationFailure "Expected error when using non-existent type in existing schema, but statement succeeded"
