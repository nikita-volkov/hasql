module Integration.Sharing.ErrorsSpec (spec) where

import Data.Either
import Data.Functor.Contravariant
import Data.Int (Int64)
import Data.Vector qualified as Vector
import Hasql.Connection qualified as Connection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Errors qualified as Errors
import Hasql.Pipeline qualified as Pipeline
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Helpers.Scripts qualified as Scripts
import Prelude
import Test.Hspec

spec :: SpecWith Scripts.ScopeParams
spec = do
  describe "Client-side rejections (#327)" do
    -- libpq rejects `sendQueryParams`/`sendQueryPrepared` outright when given
    -- more than 65535 parameters, without ever talking to the server. Unlike
    -- an actual connection loss, the exact same request fails identically on
    -- every retry, so it must not be classified the same way as one.
    it "reports too many parameters as ClientRejectionSessionError, not the transient ConnectionSessionError" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        let tooManyParams = 65536
            statement =
              Statement.unpreparable
                "select 1"
                (mconcat (replicate tooManyParams (contramap (const (0 :: Int64)) (Encoders.param (Encoders.nonNullable Encoders.int8)))))
                Decoders.noResult
        result <- Connection.use connection (Session.statement () statement)
        case result of
          Left err@(Errors.ClientRejectionSessionError _) ->
            Errors.isTransient err `shouldBe` False
          Left err ->
            expectationFailure ("Expected ClientRejectionSessionError, but got: " <> show err)
          Right () ->
            expectationFailure "Expected the statement to be rejected for having too many parameters"

  describe "Syntax errors" do
    forM_ [False, True] \inPipeline -> do
      describe (if inPipeline then "Pipeline" else "Session") do
        forM_ [False, True] \preparable -> do
          describe (if preparable then "Preparable" else "Unpreparable") do
            it "gets reported properly" \config -> do
              Scripts.onPreparableConnection config \connection -> do
                result <- Connection.use connection do
                  let statement =
                        if preparable
                          then Statement.preparable "-" mempty Decoders.noResult
                          else Statement.unpreparable "-" mempty Decoders.noResult
                  if inPipeline
                    then Session.pipeline (Pipeline.statement () statement)
                    else Session.statement () statement

                shouldBe
                  result
                  ( Left
                      ( (Errors.StatementSessionError 1 0 "-" [] preparable)
                          ( Errors.ServerStatementError
                              ( Errors.ServerError
                                  "42601"
                                  "syntax error at or near \"-\""
                                  Nothing
                                  Nothing
                                  (Just 1)
                              )
                          )
                      )
                  )

  describe "Decoder mismatches" $ parallel do
    decoderMismatchByPreparedStatusAndExecutor True "Session" (Session.statement ())
    decoderMismatchByPreparedStatusAndExecutor False "Session" (Session.statement ())
    decoderMismatchByPreparedStatusAndExecutor True "Pipeline" (Session.pipeline . Pipeline.statement ())
    decoderMismatchByPreparedStatusAndExecutor False "Pipeline" (Session.pipeline . Pipeline.statement ())

decoderMismatchByPreparedStatusAndExecutor ::
  Bool ->
  Text ->
  (forall a. (Show a) => Statement.Statement () a -> Session.Session a) ->
  SpecWith Scripts.ScopeParams
decoderMismatchByPreparedStatusAndExecutor preparable executorName executor = do
  describe (if preparable then "Preparable" else "Unpreparable") do
    describe (toList executorName) do
      describe "UnexpectedColumnCount" do
        it "gets reported when result has more columns" \config -> do
          Scripts.onPreparableConnection config \connection -> do
            let statement =
                  (if preparable then Statement.preparable else Statement.unpreparable)
                    "select 1, 2"
                    mempty
                    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
            result <- Connection.use connection (executor statement)
            case result of
              Left (Errors.StatementSessionError _ _ _ _ _ (Errors.UnexpectedColumnCountStatementError expected actual)) -> do
                shouldBe expected 1
                shouldBe actual 2
              Left err ->
                expectationFailure ("Unexpected type of error: " <> show err)
              result ->
                expectationFailure ("Not an error: " <> show result)

        it "gets reported when result has fewer columns" \config -> do
          Scripts.onPreparableConnection config \connection -> do
            let statement =
                  (if preparable then Statement.preparable else Statement.unpreparable)
                    "select 1"
                    mempty
                    ( Decoders.singleRow
                        ( (,)
                            <$> Decoders.column (Decoders.nonNullable Decoders.int8)
                            <*> Decoders.column (Decoders.nonNullable Decoders.int8)
                        )
                    )
            result <- Connection.use connection (executor statement)
            case result of
              Left (Errors.StatementSessionError _ _ _ _ _ (Errors.UnexpectedColumnCountStatementError expected actual)) -> do
                shouldBe expected 2
                shouldBe actual 1
              Left err ->
                expectationFailure ("Unexpected type of error: " <> show err)
              result ->
                expectationFailure ("Not an error: " <> show result)

      describe "DecoderTypeMismatch" do
        describe "singleRow" do
          it "gets reported when column type mismatches decoder" \config -> do
            Scripts.onPreparableConnection config \connection -> do
              let statement =
                    (if preparable then Statement.preparable else Statement.unpreparable)
                      "select 1::int8, 'text'::text"
                      mempty
                      ( Decoders.singleRow
                          ( (,)
                              <$> Decoders.column (Decoders.nonNullable Decoders.int8)
                              <*> Decoders.column (Decoders.nonNullable Decoders.int8)
                          )
                      )
              result <- Connection.use connection (executor statement)
              case result of
                Left (Errors.StatementSessionError _ _ _ _ _ (Errors.UnexpectedColumnTypeStatementError column expected actual)) -> do
                  shouldBe column 1
                  shouldBe expected 20
                  shouldBe actual 25
                Left err ->
                  expectationFailure ("Unexpected type of error: " <> show err)
                result ->
                  expectationFailure ("Not an error: " <> show result)

        describe "rowMaybe" do
          it "gets reported when column type mismatches decoder" \config -> do
            Scripts.onPreparableConnection config \connection -> do
              let statement =
                    (if preparable then Statement.preparable else Statement.unpreparable)
                      "select 1::int8, 'text'::text"
                      mempty
                      ( Decoders.rowMaybe
                          ( (,)
                              <$> Decoders.column (Decoders.nonNullable Decoders.int8)
                              <*> Decoders.column (Decoders.nonNullable Decoders.int8)
                          )
                      )
              result <- Connection.use connection (executor statement)
              case result of
                Left (Errors.StatementSessionError _ _ _ _ _ (Errors.UnexpectedColumnTypeStatementError column expected actual)) -> do
                  shouldBe column 1
                  (expected, actual) `shouldBe` (20, 25)
                Left err ->
                  expectationFailure ("Unexpected type of error: " <> show err)
                result ->
                  expectationFailure ("Not an error: " <> show result)

        describe "rowVector" do
          it "gets reported when column type mismatches decoder" \config -> do
            Scripts.onPreparableConnection config \connection -> do
              let statement =
                    (if preparable then Statement.preparable else Statement.unpreparable)
                      "select int8 '1', text 'text'"
                      mempty
                      ( Decoders.rowVector
                          ( (,)
                              <$> Decoders.column (Decoders.nonNullable Decoders.int8)
                              <*> Decoders.column (Decoders.nonNullable Decoders.int8)
                          )
                      )
              result <- Connection.use connection (executor statement)
              case result of
                Left (Errors.StatementSessionError _ _ _ _ _ (Errors.UnexpectedColumnTypeStatementError column expected actual)) -> do
                  shouldBe column 1
                  (expected, actual) `shouldBe` (20, 25)
                Left err ->
                  expectationFailure ("Unexpected type of error: " <> show err)
                result ->
                  expectationFailure ("Not an error: " <> show result)

        describe "array" do
          describe "decoder:int8[]" do
            describe "column:int8" do
              it "reports properly" \config -> do
                Scripts.onPreparableConnection config \connection -> do
                  let statement =
                        (if preparable then Statement.preparable else Statement.unpreparable)
                          "select 1::int8"
                          mempty
                          ( Decoders.singleRow
                              (Decoders.column (Decoders.nonNullable (Decoders.vectorArray @Vector (Decoders.nonNullable Decoders.int8))))
                          )
                  result <- Connection.use connection (executor statement)
                  case result of
                    Left (Errors.StatementSessionError _ _ _ _ _ (Errors.UnexpectedColumnTypeStatementError column expected actual)) -> do
                      shouldBe column 0
                      (expected, actual) `shouldBe` (1016, 20)
                    Left err ->
                      expectationFailure ("Unexpected type of error: " <> show err)
                    result ->
                      expectationFailure ("Not an error: " <> show result)

          describe "decoder:int8[]" do
            describe "column:int8[]" do
              it "decodes properly" \config -> do
                Scripts.onPreparableConnection config \connection -> do
                  let statement =
                        (if preparable then Statement.preparable else Statement.unpreparable)
                          "select ARRAY[1::int8, 2::int8]"
                          mempty
                          ( Decoders.singleRow
                              (Decoders.column (Decoders.nonNullable (Decoders.vectorArray @Vector (Decoders.nonNullable Decoders.int8))))
                          )
                  result <- Connection.use connection (executor statement)
                  shouldBe result (Right (Vector.fromList [1, 2]))

          describe "decoder:int8" do
            describe "column:int8[]" do
              it "reports properly" \config -> do
                Scripts.onPreparableConnection config \connection -> do
                  let statement =
                        (if preparable then Statement.preparable else Statement.unpreparable)
                          "select ARRAY[1::int8, 2::int8]"
                          mempty
                          ( Decoders.singleRow
                              (Decoders.column (Decoders.nonNullable Decoders.int8))
                          )
                  result <- Connection.use connection (executor statement)
                  case result of
                    Left (Errors.StatementSessionError _ _ _ _ _ (Errors.UnexpectedColumnTypeStatementError column expected actual)) -> do
                      shouldBe column 0
                      (expected, actual) `shouldBe` (20, 1016)
                    Left err ->
                      expectationFailure ("Unexpected type of error: " <> show err)
                    result ->
                      expectationFailure ("Not an error: " <> show result)
