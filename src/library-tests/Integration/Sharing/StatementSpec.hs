module Integration.Sharing.StatementSpec (spec) where

import Data.Either
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Errors qualified as Errors
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Dsls.Execution qualified as Execution
import Helpers.Scripts qualified as Scripts
import Helpers.Statements.CountPreparedStatements qualified as CountPreparedStatements
import Prelude
import Test.Hspec

spec :: SpecWith Scripts.ScopeParams
spec = do
  describe "Statement Functionality" do
    describe "Prepared statements" do
      it "allows reuse of the same prepared statement on different types" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement1 =
                Statement.preparable
                  "select $1"
                  (Encoders.param (Encoders.nonNullable Encoders.text))
                  (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
          let statement2 =
                Statement.preparable
                  "select $1"
                  (Encoders.param (Encoders.nonNullable Encoders.int8))
                  (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))

          result <-
            Connection.use connection do
              result1 <- Session.statement "ok" statement1
              result2 <- Session.statement (1 :: Int64) statement2
              return (result1, result2)
          result `shouldBe` Right ("ok", 1 :: Int64)

    describe "Row counting" do
      it "counts affected rows correctly" \config -> do
        tableName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          let dropTable = Statement.preparable ("drop table if exists " <> tableName) mempty Decoders.noResult
          let createTable = Statement.preparable ("create table " <> tableName <> " (id bigserial not null, name varchar not null, primary key (id))") mempty Decoders.noResult
          let insertRow = Statement.unpreparable ("insert into " <> tableName <> " (name) values ('a')") mempty Decoders.noResult
          let deleteRows = Statement.unpreparable ("delete from " <> tableName) mempty Decoders.rowsAffected

          result <-
            Connection.use connection do
              Session.statement () dropTable
              Session.statement () createTable
              replicateM_ 100 (Session.statement () insertRow)
              affectedRows <- Session.statement () deleteRows
              Session.statement () dropTable
              return affectedRows
          result `shouldBe` Right 100

    describe "Auto-incremented columns" do
      it "returns auto-incremented column results" \config -> do
        tableName <- Scripts.generateSymname
        Scripts.onPreparableConnection config \connection -> do
          let dropTable = Statement.preparable ("drop table if exists " <> tableName) mempty Decoders.noResult
          let createTable = Statement.preparable ("create table " <> tableName <> " (id bigserial not null, name varchar not null, primary key (id))") mempty Decoders.noResult
          let insertRow = Statement.unpreparable ("insert into " <> tableName <> " (name) values ('a') returning id") mempty (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
          let insertRow2 = Statement.unpreparable ("insert into " <> tableName <> " (name) values ('b') returning id") mempty (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))

          result <-
            Connection.use connection do
              Session.statement () dropTable
              Session.statement () createTable
              id1 <- Session.statement () insertRow
              id2 <- Session.statement () insertRow2
              Session.statement () dropTable
              return (id1, id2)
          result `shouldBe` Right (1 :: Int64, 2 :: Int64)

    describe "List decoding" do
      it "decodes lists correctly" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.preparable
                  "values (1 :: int8, 2 :: int8), (3,4), (5,6)"
                  mempty
                  (Decoders.rowList ((,) <$> (Decoders.column (Decoders.nonNullable Decoders.int8)) <*> (Decoders.column (Decoders.nonNullable Decoders.int8))))
          result <- Connection.use connection (Session.statement () statement)
          result `shouldBe` Right [(1 :: Int64, 2 :: Int64), (3, 4), (5, 6)]

    describe "IN simulation" do
      it "works with arrays" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.preparable
                  "select true where 1 = any ($1)"
                  (Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable Encoders.int8))))))
                  (fmap (maybe False (const True)) (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.bool))))
          result <- Connection.use connection do
            result1 <- Session.statement ([1, 2] :: [Int64]) statement
            result2 <- Session.statement ([2, 3] :: [Int64]) statement
            return (result1, result2)
          result `shouldBe` Right (True, False)

    describe "NOT IN simulation" do
      it "works with arrays" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          let statement =
                Statement.preparable
                  "select true where 3 <> all ($1)"
                  (Encoders.param (Encoders.nonNullable (Encoders.array (Encoders.dimension foldl' (Encoders.element (Encoders.nonNullable Encoders.int8))))))
                  (fmap (maybe False (const True)) (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.bool))))
          result <- Connection.use connection do
            result1 <- Session.statement ([1, 2] :: [Int64]) statement
            result2 <- Session.statement ([2, 3] :: [Int64]) statement
            return (result1, result2)
          result `shouldBe` Right (True, False)

    describe "Preparation" do
      it "Do get prepared when configuration allows" \config -> do
        Scripts.onPreparableConnection config \connection -> do
          -- Execute a preparable statement
          result <-
            Connection.use connection do
              Session.statement
                ()
                ( Statement.preparable
                    "select 1 + 1"
                    mempty
                    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                )
          result `shouldBe` Right 2

          -- Query pg_prepared_statements to verify it was prepared
          preparedCount <-
            Connection.use connection do
              Execution.sessionByParams CountPreparedStatements.CountPreparedStatements

          preparedCount `shouldSatisfy` \case
            Right count -> count > 0
            Left _ -> False

      it "Do not get prepared when configuration forbids it" \config -> do
        Scripts.onUnpreparableConnection config \connection -> do
          -- Execute a statement marked as preparable
          result <-
            Connection.use connection do
              Session.statement
                ()
                ( Statement.preparable
                    "select 2 + 2"
                    mempty
                    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                )
          result `shouldBe` Right 4

          -- Query pg_prepared_statements to verify it was NOT prepared
          preparedCount <-
            Connection.use connection do
              Execution.sessionByParams CountPreparedStatements.CountPreparedStatements

          preparedCount `shouldBe` Right 0

    describe "Cache resilience after a failing prepared statement" do
      describe "Session" do
        it "Failing statements don't cause misses in updates of the prepared statement cache" \config -> do
          Scripts.onPreparableConnection config \connection -> do
            -- Run an intentionally failing prepared statement to set the condition of the bug.
            result <- Connection.use connection do
              Session.statement
                ()
                ( Statement.preparable
                    "select null"
                    mempty
                    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                )
            shouldBe (isLeft result) True
            -- Run a succeeding prepared statement to see if the cache is still in a good state.
            result <- Connection.use connection do
              Session.statement
                ()
                ( Statement.preparable
                    "select 1"
                    mempty
                    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                )
            -- If there is an error the cache got corrupted.
            case result of
              Right _ ->
                pure ()
              Left result ->
                expectationFailure ("Unexpected error: " <> show result)

        it "Syntax errors in prepared statements don't corrupt the cache for subsequent uses of the same statement" \config -> do
          Scripts.onPreparableConnection config \connection -> do
            let brokenStatement =
                  Statement.preparable
                    "S"
                    mempty
                    Decoders.noResult
            -- First run: syntax error.
            result1 <- Connection.use connection do
              Session.statement () brokenStatement
            error1 <- case result1 of
              Left error1 -> pure error1
              Right _ -> fail "First run unexpectedly succeeded"

            -- Second run of the same statement: should also produce a syntax error,
            -- not "prepared statement does not exist".
            result2 <- Connection.use connection do
              Session.statement () brokenStatement
            error2 <- case result2 of
              Left error2 -> pure error2
              Right _ -> fail "Second run unexpectedly succeeded"
            shouldBe error2 error1

      describe "Pipeline" do
        it "Failing pipeline statements don't cause misses in updates of the prepared statement cache" \config -> do
          Scripts.onPreparableConnection config \connection -> do
            -- Run an intentionally failing prepared statement in a pipeline to set the condition of the bug.
            result <- Connection.use connection do
              Session.pipeline do
                Pipeline.statement
                  ()
                  ( Statement.preparable
                      "select null :: int4"
                      mempty
                      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                  )
            case result of
              Right val ->
                expectationFailure ("First statement succeeded unexpectedly: " <> show val)
              Left _ ->
                pure ()

            -- Run a succeeding prepared statement in a pipeline to see if the cache is still in a good state.
            result <- Connection.use connection do
              Session.pipeline do
                Pipeline.statement
                  ()
                  ( Statement.preparable
                      "select 1"
                      mempty
                      (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                  )
            -- If there is an error the cache got corrupted.
            case result of
              Right _ ->
                pure ()
              Left result ->
                expectationFailure ("Unexpected error: " <> show result)

        it "Syntax errors in pipeline prepared statements don't corrupt the cache for subsequent uses of the same statement" \config -> do
          Scripts.onPreparableConnection config \connection -> do
            let brokenStatement =
                  Statement.preparable
                    "S"
                    mempty
                    Decoders.noResult
            -- First run: syntax error.
            result1 <- Connection.use connection do
              Session.pipeline (Pipeline.statement () brokenStatement)
            shouldBe (isLeft result1) True
            -- Second run of the same statement: should also produce a syntax error,
            -- not "prepared statement does not exist".
            result2 <- Connection.use connection do
              Session.pipeline (Pipeline.statement () brokenStatement)
            case result2 of
              Left (Errors.StatementSessionError _ _ _ _ _ (Errors.ServerStatementError (Errors.ServerError "42601" _ _ _ _))) ->
                pure ()
              Left other ->
                expectationFailure ("Unexpected error on second run: " <> show other)
              Right _ ->
                expectationFailure "Second run unexpectedly succeeded"

        it "A pipeline with a broken statement first and a valid one after it can be retried with the same syntax error" \config -> do
          Scripts.onPreparableConnection config \connection -> do
            let broken = Statement.preparable "S" mempty Decoders.noResult
                ok = Statement.preparable "select 1" mempty (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
            result1 <- Connection.use connection do
              Session.pipeline do
                (,)
                  <$> Pipeline.statement () broken
                  <*> Pipeline.statement () ok
            error1 <- case result1 of
              Left error1 -> pure error1
              Right _ -> fail "First run unexpectedly succeeded"

            result2 <- Connection.use connection do
              Session.pipeline do
                (,)
                  <$> Pipeline.statement () broken
                  <*> Pipeline.statement () ok
            error2 <- case result2 of
              Left error2 -> pure error2
              Right _ -> fail "Second run unexpectedly succeeded"
            shouldBe error2 error1

        it "A valid statement after a broken pipeline statement still prepares in a later pipeline" \config -> do
          Scripts.onPreparableConnection config \connection -> do
            let broken = Statement.preparable "S" mempty Decoders.noResult
                trailing = Statement.preparable "select 1" mempty (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
            result1 <- Connection.use connection do
              Session.pipeline do
                (,)
                  <$> Pipeline.statement () broken
                  <*> Pipeline.statement () trailing
            shouldBe (isLeft result1) True

            result2 <- Connection.use connection do
              Session.pipeline do
                Pipeline.statement () trailing
            case result2 of
              Right val -> val `shouldBe` 1
              Left err -> expectationFailure ("Unexpected error on follow-up pipeline: " <> show err)

        it "A pipeline with successful statements followed by a broken one can be retried without 'already exists' errors" \config -> do
          Scripts.onPreparableConnection config \connection -> do
            let ok1 = Statement.preparable "select 1" mempty (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                ok2 = Statement.preparable "select 2" mempty (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                broken = Statement.preparable "S" mempty Decoders.noResult
            -- First run: pipeline with two OK statements and a broken one at the end.
            result1 <- Connection.use connection do
              Session.pipeline do
                (,,)
                  <$> Pipeline.statement () ok1
                  <*> Pipeline.statement () ok2
                  <*> Pipeline.statement () broken
            error1 <- case result1 of
              Left error1 -> pure error1
              Right _ -> fail "First run unexpectedly succeeded"

            -- Second run of the same pipeline: must fail with the SAME syntax error,
            -- not "prepared statement already exists".
            result2 <- Connection.use connection do
              Session.pipeline do
                (,,)
                  <$> Pipeline.statement () ok1
                  <*> Pipeline.statement () ok2
                  <*> Pipeline.statement () broken
            error2 <- case result2 of
              Left error2 -> pure error2
              Right _ -> fail "Second run unexpectedly succeeded"
            shouldBe error2 error1

            -- Also, a standalone valid statement should still work afterwards.
            result3 <- Connection.use connection do
              Session.statement () ok1
            case result3 of
              Right val -> val `shouldBe` 1
              Left err -> expectationFailure ("Unexpected error on standalone statement: " <> show err)

        it "A pipeline with a broken statement in the middle can be retried without 'already exists' errors" \config -> do
          Scripts.onPreparableConnection config \connection -> do
            let ok1 = Statement.preparable "select 1" mempty (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
                broken = Statement.preparable "S" mempty Decoders.noResult
                ok2 = Statement.preparable "select 2" mempty (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))
            -- First run: pipeline with broken statement in the middle.
            result1 <- Connection.use connection do
              Session.pipeline do
                (,,)
                  <$> Pipeline.statement () ok1
                  <*> Pipeline.statement () broken
                  <*> Pipeline.statement () ok2
            shouldBe (isLeft result1) True

            -- Second run of the same pipeline: must fail with the same syntax error.
            result2 <- Connection.use connection do
              Session.pipeline do
                (,,)
                  <$> Pipeline.statement () ok1
                  <*> Pipeline.statement () broken
                  <*> Pipeline.statement () ok2
            case result2 of
              Left (Errors.StatementSessionError _ _ _ _ _ (Errors.ServerStatementError (Errors.ServerError "42601" _ _ _ _))) ->
                pure ()
              Left other ->
                expectationFailure ("Unexpected error on second run: " <> show other)
              Right _ ->
                expectationFailure "Second run unexpectedly succeeded"

            -- Standalone valid statements should still work afterwards.
            result3 <- Connection.use connection do
              Session.statement () ok1
            case result3 of
              Right val -> val `shouldBe` 1
              Left err -> expectationFailure ("Unexpected error on standalone ok1: " <> show err)
            result4 <- Connection.use connection do
              Session.statement () ok2
            case result4 of
              Right val -> val `shouldBe` 2
              Left err -> expectationFailure ("Unexpected error on standalone ok2: " <> show err)

    describe "Decoder compatibility cache" $ parallel do
      decoderCompatibilityCacheByExecutor "Session" (Session.statement ())
      decoderCompatibilityCacheByExecutor "Pipeline" (Session.pipeline . Pipeline.statement ())

decoderCompatibilityCacheByExecutor ::
  Text ->
  (forall a. (Show a) => Statement.Statement () a -> Session.Session a) ->
  SpecWith Scripts.ScopeParams
decoderCompatibilityCacheByExecutor executorName executor = do
  describe (toList executorName) do
    it "does not hide decoder mismatches from a previously verified statement" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let sql = "select 1::int8, 'text'::text"
            correctStatement =
              Statement.preparable
                sql
                mempty
                ( Decoders.singleRow
                    ( (,)
                        <$> Decoders.column (Decoders.nonNullable Decoders.int8)
                        <*> Decoders.column (Decoders.nonNullable Decoders.text)
                    )
                )
            mismatchingStatement =
              Statement.preparable
                sql
                mempty
                ( Decoders.singleRow
                    ( (,)
                        <$> Decoders.column (Decoders.nonNullable Decoders.int8)
                        <*> Decoders.column (Decoders.nonNullable Decoders.int8)
                    )
                )
        firstResult <- Connection.use connection (executor correctStatement)
        shouldBe firstResult (Right (1, "text"))
        secondResult <- Connection.use connection (executor mismatchingStatement)
        case secondResult of
          Left (Errors.StatementSessionError _ _ _ _ _ (Errors.UnexpectedColumnTypeStatementError column expected actual)) -> do
            shouldBe column 1
            (expected, actual) `shouldBe` (20, 25)
          Left err ->
            expectationFailure ("Unexpected type of error: " <> show err)
          result ->
            expectationFailure ("Not an error: " <> show result)
