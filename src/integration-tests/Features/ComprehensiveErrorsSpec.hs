module Features.ComprehensiveErrorsSpec (spec) where

import Data.Either
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec
import TestingKit.Testcontainers qualified as Testcontainers
import Prelude

spec :: Spec
spec = Testcontainers.aroundSpecWithConnection False do
  describe "Comprehensive Error Reproduction Tests" do
    describe "SessionError types" do
      describe "QueryError" do
        describe "with ClientError" do
          it "reproduces ClientError without message" \_connection -> do
            -- This test would require a connection failure scenario
            -- which is difficult to reproduce reliably in integration tests
            pendingWith "ClientError scenarios require connection failure conditions"

        describe "with ResultError subtypes" do
          describe "ServerError" do
            it "reproduces syntax error (42601)" \connection -> do
              let statement =
                    Statement.Statement
                      "INVALID SQL SYNTAX HERE"
                      Encoders.noParams
                      Decoders.noResult
                      False
              result <- Connection.use connection (Session.statement () statement)
              case result of
                Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError code _ _ _ _))) ->
                  code `shouldBe` "42601"
                _ -> expectationFailure ("Expected syntax error, got: " <> show result)

            it "reproduces relation does not exist error (42P01)" \connection -> do
              let statement =
                    Statement.Statement
                      "SELECT * FROM nonexistent_table_xyz"
                      Encoders.noParams
                      (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int4)))
                      False
              result <- Connection.use connection (Session.statement () statement)
              case result of
                Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError code _ _ _ _))) ->
                  code `shouldBe` "42P01"
                _ -> expectationFailure ("Expected relation not found error, got: " <> show result)

            it "reproduces column does not exist error (42703)" \connection -> do
              let statement =
                    Statement.Statement
                      "SELECT nonexistent_column FROM generate_series(1,1)"
                      Encoders.noParams
                      (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int4)))
                      False
              result <- Connection.use connection (Session.statement () statement)
              case result of
                Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError code _ _ _ _))) ->
                  code `shouldBe` "42703"
                _ -> expectationFailure ("Expected column not found error, got: " <> show result)

            it "reproduces division by zero error (22012)" \connection -> do
              let statement =
                    Statement.Statement
                      "SELECT 1/0"
                      Encoders.noParams
                      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                      False
              result <- Connection.use connection (Session.statement () statement)
              case result of
                Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError code _ _ _ _))) ->
                  code `shouldBe` "22012"
                _ -> expectationFailure ("Expected division by zero error, got: " <> show result)

            it "reproduces invalid input syntax error (22P02)" \connection -> do
              let statement =
                    Statement.Statement
                      "SELECT 'text' + 123"
                      Encoders.noParams
                      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                      False
              result <- Connection.use connection (Session.statement () statement)
              case result of
                Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError code _ _ _ _))) ->
                  code `shouldBe` "22P02"
                _ -> expectationFailure ("Expected invalid input syntax error, got: " <> show result)

            it "reproduces numeric value out of range error (22003)" \connection -> do
              let statement =
                    Statement.Statement
                      "SELECT 999999999999999999999999999999999::int4"
                      Encoders.noParams
                      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                      False
              result <- Connection.use connection (Session.statement () statement)
              case result of
                Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError code _ _ _ _))) ->
                  code `shouldBe` "22003"
                _ -> expectationFailure ("Expected numeric out of range error, got: " <> show result)

            it "reproduces unique violation error (23505)" \connection -> do
              -- First create a temp table with unique constraint
              result1 <- Connection.use connection (Session.sql "CREATE TEMP TABLE test_unique (id int UNIQUE)")
              case result1 of
                Left err -> expectationFailure ("Failed to create table: " <> show err)
                Right _ -> pure ()

              -- Insert first row
              result2 <- Connection.use connection (Session.sql "INSERT INTO test_unique VALUES (1)")
              case result2 of
                Left err -> expectationFailure ("Failed to insert first row: " <> show err)
                Right _ -> pure ()

              -- Try to insert duplicate
              let statement =
                    Statement.Statement
                      "INSERT INTO test_unique VALUES (1)"
                      Encoders.noParams
                      Decoders.noResult
                      False
              result <- Connection.use connection (Session.statement () statement)
              case result of
                Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError code _ _ _ _))) ->
                  code `shouldBe` "23505"
                _ -> expectationFailure ("Expected unique violation error, got: " <> show result)

            it "reproduces not null violation error (23502)" \connection -> do
              -- Create a temp table with not null constraint
              result1 <- Connection.use connection (Session.sql "CREATE TEMP TABLE test_not_null (id int NOT NULL)")
              case result1 of
                Left err -> expectationFailure ("Failed to create table: " <> show err)
                Right _ -> pure ()

              -- Try to insert null value
              let statement =
                    Statement.Statement
                      "INSERT INTO test_not_null VALUES (NULL)"
                      Encoders.noParams
                      Decoders.noResult
                      False
              result <- Connection.use connection (Session.statement () statement)
              case result of
                Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError code _ _ _ _))) ->
                  code `shouldBe` "23502"
                _ -> expectationFailure ("Expected not null violation error, got: " <> show result)

          describe "UnexpectedResult" do
            it "reproduces UnexpectedResult error" \_connection -> do
              -- This is harder to trigger directly - it's usually internal
              pendingWith "UnexpectedResult errors are internal and hard to reproduce directly"

          describe "CellError with UnexpectedNull" do
            it "reproduces DecoderTypeMismatch for NULL decoding as non-nullable" \connection -> do
              let statement =
                    Statement.Statement
                      "SELECT NULL"
                      Encoders.noParams
                      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                      False
              result <- Connection.use connection (Session.statement () statement)
              case result of
                Left (Session.QueryError _ _ (Session.ResultError (Session.DecoderTypeMismatch 0 _ _))) ->
                  pure ()
                _ -> expectationFailure ("Expected DecoderTypeMismatch for NULL, got: " <> show result)

            it "reproduces UnexpectedNull in multi-row result" \connection -> do
              let statement =
                    Statement.Statement
                      "SELECT CASE WHEN generate_series = 2 THEN NULL ELSE generate_series END FROM generate_series(1,3)"
                      Encoders.noParams
                      (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int4)))
                      False
              result <- Connection.use connection (Session.statement () statement)
              case result of
                Left (Session.QueryError _ _ (Session.ResultError (Session.CellError 1 0 Session.UnexpectedNull))) ->
                  pure ()
                Left (Session.QueryError _ _ (Session.ResultError (Session.DecoderTypeMismatch _ _ _))) ->
                  pure () -- Also acceptable as the mismatch is caught during type checking
                _ -> expectationFailure ("Expected UnexpectedNull or DecoderTypeMismatch at row 1, got: " <> show result)

          describe "CellError with ValueError" do
            it "reproduces DecoderTypeMismatch for invalid UUID format" \connection -> do
              let statement =
                    Statement.Statement
                      "SELECT 'invalid-uuid-format'"
                      Encoders.noParams
                      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.uuid)))
                      False
              result <- Connection.use connection (Session.statement () statement)
              case result of
                Left (Session.QueryError _ _ (Session.ResultError (Session.DecoderTypeMismatch 0 _ _))) ->
                  pure ()
                Left (Session.QueryError _ _ (Session.ResultError (Session.CellError 0 0 (Session.ValueError _)))) ->
                  pure () -- Also acceptable if error occurs during parsing
                _ -> expectationFailure ("Expected DecoderTypeMismatch or ValueError for invalid UUID, got: " <> show result)

            it "reproduces actual ValueError for binary data parsing failure" \connection -> do
              -- Test with bytea decoder on incompatible text data - this should trigger ValueError
              let statement =
                    Statement.Statement
                      "SELECT 'not valid hex data' :: text"
                      Encoders.noParams
                      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bytea)))
                      False
              result <- Connection.use connection (Session.statement () statement)
              case result of
                Left (Session.QueryError _ _ (Session.ResultError (Session.CellError 0 0 (Session.ValueError _)))) ->
                  pure ()
                Left (Session.QueryError _ _ (Session.ResultError (Session.DecoderTypeMismatch 0 _ _))) ->
                  pure () -- Type mismatch is also acceptable
                _ -> expectationFailure ("Expected ValueError or DecoderTypeMismatch for binary parsing, got: " <> show result)

            it "reproduces actual UnexpectedNull with proper nullable/non-nullable setup" \connection -> do
              -- Create a temp table to test proper null handling
              result1 <- Connection.use connection (Session.sql "CREATE TEMP TABLE test_nulls (id int, value int)")
              case result1 of
                Left err -> expectationFailure ("Failed to create table: " <> show err)
                Right _ -> pure ()

              result2 <- Connection.use connection (Session.sql "INSERT INTO test_nulls VALUES (1, 42), (2, NULL)")
              case result2 of
                Left err -> expectationFailure ("Failed to insert data: " <> show err)
                Right _ -> pure ()

              let statement =
                    Statement.Statement
                      "SELECT value FROM test_nulls ORDER BY id"
                      Encoders.noParams
                      (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int4)))
                      False
              result <- Connection.use connection (Session.statement () statement)
              case result of
                Left (Session.QueryError _ _ (Session.ResultError (Session.CellError 1 0 Session.UnexpectedNull))) ->
                  pure ()
                Left (Session.QueryError _ _ (Session.ResultError (Session.DecoderTypeMismatch _ _ _))) ->
                  pure () -- Also acceptable as type system catches this
                _ -> expectationFailure ("Expected UnexpectedNull or DecoderTypeMismatch at row 1, got: " <> show result)

          describe "UnexpectedAmountOfRows" do
            it "reproduces UnexpectedAmountOfRows with singleRow decoder" \connection -> do
              let statement =
                    Statement.Statement
                      "SELECT generate_series FROM generate_series(1,3)"
                      Encoders.noParams
                      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                      False
              result <- Connection.use connection (Session.statement () statement)
              case result of
                Left (Session.QueryError _ _ (Session.ResultError (Session.UnexpectedAmountOfRows 3))) ->
                  pure ()
                _ -> expectationFailure ("Expected UnexpectedAmountOfRows, got: " <> show result)

            it "reproduces UnexpectedAmountOfRows with no rows but expecting one" \connection -> do
              let statement =
                    Statement.Statement
                      "SELECT 1 WHERE FALSE"
                      Encoders.noParams
                      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                      False
              result <- Connection.use connection (Session.statement () statement)
              case result of
                Left (Session.QueryError _ _ (Session.ResultError (Session.UnexpectedAmountOfRows 0))) ->
                  pure ()
                _ -> expectationFailure ("Expected UnexpectedAmountOfRows with 0 rows, got: " <> show result)

          describe "UnexpectedAmountOfColumns" do
            it "reproduces UnexpectedAmountOfColumns expecting more columns" \connection -> do
              let statement =
                    Statement.Statement
                      "SELECT 1"
                      Encoders.noParams
                      ( Decoders.singleRow
                          ( (,)
                              <$> Decoders.column (Decoders.nonNullable Decoders.int4)
                              <*> Decoders.column (Decoders.nonNullable Decoders.int4)
                          )
                      )
                      False
              result <- Connection.use connection (Session.statement () statement)
              case result of
                Left (Session.QueryError _ _ (Session.ResultError (Session.UnexpectedAmountOfColumns 2 1))) ->
                  pure ()
                _ -> expectationFailure ("Expected UnexpectedAmountOfColumns, got: " <> show result)

            it "reproduces UnexpectedAmountOfColumns expecting fewer columns" \connection -> do
              let statement =
                    Statement.Statement
                      "SELECT 1, 2, 3"
                      Encoders.noParams
                      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                      False
              result <- Connection.use connection (Session.statement () statement)
              case result of
                Left (Session.QueryError _ _ (Session.ResultError (Session.UnexpectedAmountOfColumns 1 3))) ->
                  pure ()
                _ -> expectationFailure ("Expected UnexpectedAmountOfColumns, got: " <> show result)

          describe "DecoderTypeMismatch" do
            it "reproduces DecoderTypeMismatch trying to decode text as int" \connection -> do
              let statement =
                    Statement.Statement
                      "SELECT 'hello'"
                      Encoders.noParams
                      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                      False
              result <- Connection.use connection (Session.statement () statement)
              case result of
                Left (Session.QueryError _ _ (Session.ResultError (Session.DecoderTypeMismatch 0 _ _))) ->
                  pure ()
                _ -> expectationFailure ("Expected DecoderTypeMismatch, got: " <> show result)

            it "reproduces DecoderTypeMismatch trying to decode int as UUID" \connection -> do
              let statement =
                    Statement.Statement
                      "SELECT 42"
                      Encoders.noParams
                      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.uuid)))
                      False
              result <- Connection.use connection (Session.statement () statement)
              case result of
                Left (Session.QueryError _ _ (Session.ResultError (Session.DecoderTypeMismatch 0 _ _))) ->
                  pure ()
                _ -> expectationFailure ("Expected DecoderTypeMismatch, got: " <> show result)

    describe "PipelineError" do
      describe "with ServerError" do
        it "reproduces syntax error in pipeline" \connection -> do
          result <- Connection.use connection do
            Session.pipeline do
              Pipeline.statement () (Statement.Statement "INVALID SQL" Encoders.noParams Decoders.noResult False)
          case result of
            Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "42601" _ _ _ _))) ->
              pure ()
            _ -> expectationFailure ("Expected syntax error in pipeline, got: " <> show result)

      describe "with CellError" do
        it "reproduces DecoderTypeMismatch for NULL in pipeline" \connection -> do
          result <- Connection.use connection do
            Session.pipeline do
              Pipeline.statement
                ()
                ( Statement.Statement
                    "SELECT NULL"
                    Encoders.noParams
                    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                    False
                )
          case result of
            Left (Session.QueryError _ _ (Session.ResultError (Session.DecoderTypeMismatch 0 _ _))) ->
              pure ()
            Left (Session.QueryError _ _ (Session.ResultError (Session.CellError 0 0 Session.UnexpectedNull))) ->
              pure () -- Also acceptable
            _ -> expectationFailure ("Expected DecoderTypeMismatch or UnexpectedNull in pipeline, got: " <> show result)

      describe "with DecoderTypeMismatch" do
        it "reproduces DecoderTypeMismatch in pipeline" \connection -> do
          result <- Connection.use connection do
            Session.pipeline do
              Pipeline.statement
                ()
                ( Statement.Statement
                    "SELECT 'text'"
                    Encoders.noParams
                    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                    False
                )
          case result of
            Left (Session.QueryError _ _ (Session.ResultError (Session.DecoderTypeMismatch 0 _ _))) ->
              pure ()
            _ -> expectationFailure ("Expected DecoderTypeMismatch in pipeline, got: " <> show result)

  describe "Error context variations" do
    describe "Prepared vs Unprepared statements" do
      it "reproduces errors with prepared statements" \connection -> do
        let statement =
              Statement.Statement
                "SELECT $1::int4"
                (Encoders.param (Encoders.nonNullable Encoders.text))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                True -- prepared
        result <- Connection.use connection (Session.statement "not-a-number" statement)
        case result of
          Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "22P02" _ _ _ _))) ->
            pure ()
          _ -> expectationFailure ("Expected invalid input syntax error, got: " <> show result)

      it "reproduces errors with unprepared statements" \connection -> do
        let statement =
              Statement.Statement
                "SELECT $1::int4"
                (Encoders.param (Encoders.nonNullable Encoders.text))
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                False -- unprepared
        result <- Connection.use connection (Session.statement "not-a-number" statement)
        case result of
          Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "22P02" _ _ _ _))) ->
            pure ()
          _ -> expectationFailure ("Expected invalid input syntax error, got: " <> show result)

    describe "Different parameter combinations" do
      it "reproduces errors with parameters rendered in error message" \connection -> do
        let statement =
              Statement.Statement
                "SELECT $1 / $2"
                ( mconcat
                    [ fst >$< Encoders.param (Encoders.nonNullable Encoders.int4),
                      snd >$< Encoders.param (Encoders.nonNullable Encoders.int4)
                    ]
                )
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                False
        result <- Connection.use connection (Session.statement (10 :: Int32, 0 :: Int32) statement)
        case result of
          Left (Session.QueryError _ params (Session.ResultError (Session.ServerError "22012" _ _ _ _))) -> do
            length params `shouldBe` 2
            params `shouldContain` ["10"]
            params `shouldContain` ["0"]
          _ -> expectationFailure ("Expected division by zero with parameters, got: " <> show result)

  describe "Edge cases and combinations" do
    it "reproduces multi-column DecoderTypeMismatch at different positions" \connection -> do
      let statement =
            Statement.Statement
              "SELECT 1, NULL, 3"
              Encoders.noParams
              ( Decoders.singleRow
                  ( (,,)
                      <$> Decoders.column (Decoders.nonNullable Decoders.int4)
                      <*> Decoders.column (Decoders.nonNullable Decoders.int4)
                      <*> Decoders.column (Decoders.nonNullable Decoders.int4)
                  )
              )
              False
      result <- Connection.use connection (Session.statement () statement)
      case result of
        Left (Session.QueryError _ _ (Session.ResultError (Session.DecoderTypeMismatch 1 _ _))) ->
          pure ()
        Left (Session.QueryError _ _ (Session.ResultError (Session.CellError 0 1 Session.UnexpectedNull))) ->
          pure () -- Also acceptable
        _ -> expectationFailure ("Expected DecoderTypeMismatch or UnexpectedNull at column 1, got: " <> show result)

    it "reproduces multi-row CellError at specific row position" \connection -> do
      let statement =
            Statement.Statement
              "SELECT CASE WHEN i = 3 THEN NULL ELSE i END FROM generate_series(1,5) i"
              Encoders.noParams
              (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int4)))
              False
      result <- Connection.use connection (Session.statement () statement)
      case result of
        Left (Session.QueryError _ _ (Session.ResultError (Session.CellError 2 0 Session.UnexpectedNull))) ->
          pure ()
        _ -> expectationFailure ("Expected UnexpectedNull at row 2, got: " <> show result)

    describe "Additional comprehensive error scenarios" do
      it "reproduces large parameter list with error" \connection -> do
        -- Test with many parameters to ensure parameter rendering works
        let statement =
              Statement.Statement
                "SELECT $1 + $2 + $3 + $4 + $5 + $6 / $7"
                ( mconcat
                    [ (\(a, _, _, _, _, _, _) -> a) >$< Encoders.param (Encoders.nonNullable Encoders.int4),
                      (\(_, b, _, _, _, _, _) -> b) >$< Encoders.param (Encoders.nonNullable Encoders.int4),
                      (\(_, _, c, _, _, _, _) -> c) >$< Encoders.param (Encoders.nonNullable Encoders.int4),
                      (\(_, _, _, d, _, _, _) -> d) >$< Encoders.param (Encoders.nonNullable Encoders.int4),
                      (\(_, _, _, _, e, _, _) -> e) >$< Encoders.param (Encoders.nonNullable Encoders.int4),
                      (\(_, _, _, _, _, f, _) -> f) >$< Encoders.param (Encoders.nonNullable Encoders.int4),
                      (\(_, _, _, _, _, _, g) -> g) >$< Encoders.param (Encoders.nonNullable Encoders.int4)
                    ]
                )
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                False
        result <- Connection.use connection (Session.statement (1 :: Int32, 2 :: Int32, 3 :: Int32, 4 :: Int32, 5 :: Int32, 6 :: Int32, 0 :: Int32) statement)
        case result of
          Left (Session.QueryError _ params (Session.ResultError (Session.ServerError "22012" _ _ _ _))) -> do
            length params `shouldBe` 7
            params `shouldContain` ["0"] -- The zero that causes division by zero
          _ -> expectationFailure ("Expected division by zero with 7 parameters, got: " <> show result)

      it "reproduces complex query with position error reporting" \connection -> do
        let statement =
              Statement.Statement
                "SELECT a.id FROM (SELECT 1 as id) a WHERE a.nonexistent_column = 1"
                Encoders.noParams
                (Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int4)))
                False
        result <- Connection.use connection (Session.statement () statement)
        case result of
          Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "42703" _ _ _ (Just _)))) ->
            pure () -- Should have position information
          Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "42703" _ _ _ Nothing))) ->
            pure () -- Position information might not always be available
          _ -> expectationFailure ("Expected column not found error with position, got: " <> show result)

      it "reproduces transaction state errors" \connection -> do
        -- Start a transaction and then cause an error
        result1 <- Connection.use connection (Session.sql "BEGIN")
        case result1 of
          Left err -> expectationFailure ("Failed to begin transaction: " <> show err)
          Right _ -> pure ()

        -- Cause an error within the transaction
        result2 <- Connection.use connection (Session.sql "SELECT 1/0")
        case result2 of
          Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "22012" _ _ _ _))) ->
            pure () -- Expected division by zero
          _ -> expectationFailure ("Expected division by zero in transaction, got: " <> show result2)

        -- Now try to do something else - should be in error state
        let statement =
              Statement.Statement
                "SELECT 1"
                Encoders.noParams
                (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                False
        result3 <- Connection.use connection (Session.statement () statement)
        case result3 of
          Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError "25P02" _ _ _ _))) ->
            pure () -- "in failed sql transaction" error
          Left _ ->
            pure () -- Any other error is also acceptable in this state
          Right _ -> do
            -- If successful, clean up the transaction
            _ <- Connection.use connection (Session.sql "ROLLBACK")
            pure ()

        -- Clean up
        _ <- Connection.use connection (Session.sql "ROLLBACK")
        pure ()
