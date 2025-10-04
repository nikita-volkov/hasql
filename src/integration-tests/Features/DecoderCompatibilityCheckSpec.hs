module Features.DecoderCompatibilityCheckSpec (spec) where

import Data.Either
import Data.Vector qualified as Vector
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Error qualified as Error
import Hasql.Location qualified as Location
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec
import TestingKit.Testcontainers qualified as Testcontainers
import Prelude

spec :: Spec
spec = Testcontainers.aroundSpecWithConnection False $ parallel do
  byPreparedStatusAndExecutor True "Session" (Session.statement ())
  byPreparedStatusAndExecutor False "Session" (Session.statement ())
  byPreparedStatusAndExecutor True "Pipeline" (Session.pipeline . Pipeline.statement ())
  byPreparedStatusAndExecutor False "Pipeline" (Session.pipeline . Pipeline.statement ())

byPreparedStatusAndExecutor ::
  Bool ->
  Text ->
  (forall a. (Show a) => Statement.Statement () a -> Session.Session a) ->
  SpecWith Connection.Connection
byPreparedStatusAndExecutor preparable executorName executor = do
  describe (if preparable then "Preparable" else "Unpreparable") do
    describe (toList executorName) do
      describe "UnexpectedAmountOfColumns" do
        it "gets reported when result has more columns" \connection -> do
          let statement =
                Statement.Statement
                  "select 1, 2"
                  mempty
                  (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
                  preparable
          result <- Connection.use connection (executor statement)
          case result of
            Left (Error.UnexpectedAmountOfColumnsUsageError _ expected actual) -> do
              shouldBe expected 1
              shouldBe actual 2
            Left err ->
              expectationFailure ("Unexpected type of error: " <> show err)
            result ->
              expectationFailure ("Not an error: " <> show result)

        it "gets reported when result has fewer columns" \connection -> do
          let statement =
                Statement.Statement
                  "select 1"
                  mempty
                  ( Decoders.singleRow
                      ( (,)
                          <$> Decoders.column (Decoders.nonNullable Decoders.int8)
                          <*> Decoders.column (Decoders.nonNullable Decoders.int8)
                      )
                  )
                  preparable
          result <- Connection.use connection (executor statement)
          case result of
            Left (Error.UnexpectedAmountOfColumnsUsageError _ expected actual) -> do
              shouldBe expected 2
              shouldBe actual 1
            Left err ->
              expectationFailure ("Unexpected type of error: " <> show err)
            result ->
              expectationFailure ("Not an error: " <> show result)

      describe "DecoderTypeMismatch" do
        describe "singleRow" do
          it "gets reported when column type mismatches decoder" \connection -> do
            let statement =
                  Statement.Statement
                    "select 1::int8, 'text'"
                    mempty
                    ( Decoders.singleRow
                        ( (,)
                            <$> Decoders.column (Decoders.nonNullable Decoders.int8)
                            <*> Decoders.column (Decoders.nonNullable Decoders.int8)
                        )
                    )
                    preparable
            result <- Connection.use connection (executor statement)
            case result of
              Left (Error.CellDeserializationUsageError (Location.InResultCell _ column) oid msg) -> do
                shouldBe column 1
                (oid, msg) `shouldBe` (25, "Decoder type mismatch. Expected 20")
              Left err ->
                expectationFailure ("Unexpected type of error: " <> show err)
              result ->
                expectationFailure ("Not an error: " <> show result)

        describe "rowMaybe" do
          it "gets reported when column type mismatches decoder" \connection -> do
            let statement =
                  Statement.Statement
                    "select 1::int8, 'text'"
                    mempty
                    ( Decoders.rowMaybe
                        ( (,)
                            <$> Decoders.column (Decoders.nonNullable Decoders.int8)
                            <*> Decoders.column (Decoders.nonNullable Decoders.int8)
                        )
                    )
                    preparable
            result <- Connection.use connection (executor statement)
            case result of
              Left (Error.CellDeserializationUsageError (Location.InResultCell _ column) oid msg) -> do
                shouldBe column 1
                (oid, msg) `shouldBe` (25, "Decoder type mismatch. Expected 20")
              Left err ->
                expectationFailure ("Unexpected type of error: " <> show err)
              result ->
                expectationFailure ("Not an error: " <> show result)

        describe "rowVector" do
          it "gets reported when column type mismatches decoder" \connection -> do
            let statement =
                  Statement.Statement
                    "select 1::int8, 'text'"
                    mempty
                    ( Decoders.rowVector
                        ( (,)
                            <$> Decoders.column (Decoders.nonNullable Decoders.int8)
                            <*> Decoders.column (Decoders.nonNullable Decoders.int8)
                        )
                    )
                    preparable
            result <- Connection.use connection (executor statement)
            case result of
              Left (Error.CellDeserializationUsageError (Location.InResultCell _ column) oid msg) -> do
                shouldBe column 1
                (oid, msg) `shouldBe` (25, "Decoder type mismatch. Expected 20")
              Left err ->
                expectationFailure ("Unexpected type of error: " <> show err)
              result ->
                expectationFailure ("Not an error: " <> show result)

        describe "array" do
          describe "decoder:int8[]" do
            describe "column:int8" do
              it "reports properly" \connection -> do
                let statement =
                      Statement.Statement
                        "select 1::int8"
                        mempty
                        ( Decoders.singleRow
                            (Decoders.column (Decoders.nonNullable (Decoders.vectorArray @Vector (Decoders.nonNullable Decoders.int8))))
                        )
                        preparable
                result <- Connection.use connection (executor statement)
                case result of
                  Left (Error.CellDeserializationUsageError (Location.InResultCell _ column) oid msg) -> do
                    shouldBe column 0
                    (oid, msg) `shouldBe` (20, "Decoder type mismatch. Expected 1016")
                  Left err ->
                    expectationFailure ("Unexpected type of error: " <> show err)
                  result ->
                    expectationFailure ("Not an error: " <> show result)

          describe "decoder:int8[]" do
            describe "column:int8[]" do
              it "decodes properly" \connection -> do
                let statement =
                      Statement.Statement
                        "select ARRAY[1::int8, 2::int8]"
                        mempty
                        ( Decoders.singleRow
                            (Decoders.column (Decoders.nonNullable (Decoders.vectorArray @Vector (Decoders.nonNullable Decoders.int8))))
                        )
                        preparable
                result <- Connection.use connection (executor statement)
                shouldBe result (Right (Vector.fromList [1, 2]))

          describe "decoder:int8" do
            describe "column:int8[]" do
              it "reports properly" \connection -> do
                let statement =
                      Statement.Statement
                        "select ARRAY[1::int8, 2::int8]"
                        mempty
                        ( Decoders.singleRow
                            (Decoders.column (Decoders.nonNullable Decoders.int8))
                        )
                        preparable
                result <- Connection.use connection (executor statement)
                case result of
                  Left (Error.CellDeserializationUsageError (Location.InResultCell _ column) oid msg) -> do
                    shouldBe column 0
                    (oid, msg) `shouldBe` (1016, "Decoder type mismatch. Expected 20")
                  Left err ->
                    expectationFailure ("Unexpected type of error: " <> show err)
                  result ->
                    expectationFailure ("Not an error: " <> show result)
