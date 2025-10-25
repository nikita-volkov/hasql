module Sharing.ByUnit.Decoders.DomainSpec (spec) where

import Data.HashSet qualified as HashSet
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
  describe "Simple domains" do
    it "decodes a simple domain from static SQL" \config -> do
      domainName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create domain type
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["create domain ", domainName, " as int8 check (value > 0)"]))
              mempty
              Decoders.noResult
              True
          -- Test decoding from static value
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select 42 :: ", domainName]))
              mempty
              ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          (Decoders.domain Nothing domainName Decoders.int8)
                      )
                  )
              )
              True
        result `shouldBe` Right (42 :: Int64)

    it "decodes a text domain" \config -> do
      domainName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create text domain type
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["create domain ", domainName, " as text check (length(value) < 100)"]))
              mempty
              Decoders.noResult
              True
          -- Test decoding
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select 'hello world' :: ", domainName]))
              mempty
              ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          (Decoders.domain Nothing domainName Decoders.text)
                      )
                  )
              )
              True
        result `shouldBe` Right "hello world"

    it "decodes nullable domains" \config -> do
      domainName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create domain type
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["create domain ", domainName, " as int4 check (value > 0)"]))
              mempty
              Decoders.noResult
              True
          -- Test decoding null
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select null :: ", domainName]))
              mempty
              ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nullable
                          (Decoders.domain Nothing domainName Decoders.int4)
                      )
                  )
              )
              True
        result `shouldBe` Right (Nothing :: Maybe Int32)

  describe "Plain codec incompatibility" do
    it "fails when using plain codec for domain type column" \config -> do
      domainName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create domain type
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["create domain ", domainName, " as int8 check (value > 0)"]))
              mempty
              Decoders.noResult
              True
          -- Try to decode domain column using plain int8 decoder (should fail)
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select 42 :: ", domainName]))
              mempty
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
              True
        -- Should fail with UnexpectedColumnTypeStatementError
        case result of
          Left (Errors.StatementSessionError _ _ _ _ _ (Errors.UnexpectedColumnTypeStatementError 0 _ _)) ->
            pure ()
          Left err ->
            expectationFailure ("Unexpected type of error: " <> show err)
          Right _ ->
            expectationFailure "Expected an error but got success"

    it "succeeds when using domain codec for domain type column" \config -> do
      domainName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create domain type
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["create domain ", domainName, " as int8 check (value > 0)"]))
              mempty
              Decoders.noResult
              True
          -- Decode domain column using domain decoder (should succeed)
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select 42 :: ", domainName]))
              mempty
              ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          (Decoders.domain Nothing domainName Decoders.int8)
                      )
                  )
              )
              True
        result `shouldBe` Right (42 :: Int64)

  describe "Arrays of domains" do
    it "decodes arrays of domain values from static SQL" \config -> do
      domainName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create domain type
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["create domain ", domainName, " as int4 check (value > 0)"]))
              mempty
              Decoders.noResult
              True
          -- Test array decoding
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select array[1, 2, 3, 4, 5] :: ", domainName, "[]"]))
              mempty
              ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          ( Decoders.array
                              ( Decoders.dimension
                                  replicateM
                                  ( Decoders.element
                                      ( Decoders.nonNullable
                                          (Decoders.domain Nothing domainName Decoders.int4)
                                      )
                                  )
                              )
                          )
                      )
                  )
              )
              True
        result `shouldBe` Right [1 :: Int32, 2, 3, 4, 5]

    it "decodes 2D arrays of domain values" \config -> do
      domainName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create domain type
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["create domain ", domainName, " as int4 check (value > 0)"]))
              mempty
              Decoders.noResult
              True
          -- Test 2D array decoding
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select array[array[1, 2], array[3, 4]] :: ", domainName, "[][]"]))
              mempty
              ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          ( Decoders.array
                              ( Decoders.dimension
                                  replicateM
                                  ( Decoders.dimension
                                      replicateM
                                      ( Decoders.element
                                          ( Decoders.nonNullable
                                              (Decoders.domain Nothing domainName Decoders.int4)
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
              )
              True
        result `shouldBe` Right [[1 :: Int32, 2], [3, 4]]

    it "fails when using plain codec for domain array column" \config -> do
      domainName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create domain type
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["create domain ", domainName, " as int4 check (value > 0)"]))
              mempty
              Decoders.noResult
              True
          -- Try to decode domain array using plain int4 array decoder (should fail)
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select array[1, 2, 3] :: ", domainName, "[]"]))
              mempty
              ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          ( Decoders.array
                              ( Decoders.dimension
                                  replicateM
                                  (Decoders.element (Decoders.nonNullable Decoders.int4))
                              )
                          )
                      )
                  )
              )
              True
        -- Should fail with UnexpectedColumnTypeStatementError
        case result of
          Left (Errors.StatementSessionError _ _ _ _ _ (Errors.UnexpectedColumnTypeStatementError 0 _ _)) ->
            pure ()
          Left err ->
            expectationFailure ("Unexpected type of error: " <> show err)
          Right _ ->
            expectationFailure "Expected an error but got success"

  describe "Composites containing domains" do
    it "decodes composites with domain fields from static SQL" \config -> do
      domainName <- Scripts.generateSymname
      compositeName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create domain type
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["create domain ", domainName, " as int8 check (value > 0)"]))
              mempty
              Decoders.noResult
              True
          -- Create composite type with domain field
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["create type ", compositeName, " as (id ", domainName, ", name text)"]))
              mempty
              Decoders.noResult
              True
          -- Test decoding
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select row (42, 'test') :: ", compositeName]))
              mempty
              ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          ( Decoders.composite
                              Nothing
                              compositeName
                              ( (,)
                                  <$> Decoders.field
                                    ( Decoders.nonNullable
                                        (Decoders.domain Nothing domainName Decoders.int8)
                                    )
                                  <*> Decoders.field (Decoders.nonNullable Decoders.text)
                              )
                          )
                      )
                  )
              )
              True
        result `shouldBe` Right (42 :: Int64, "test")

    it "decodes nested composites with domain fields" \config -> do
      domainName <- Scripts.generateSymname
      innerType <- Scripts.generateSymname
      outerType <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create domain type
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["create domain ", domainName, " as int8 check (value > 0)"]))
              mempty
              Decoders.noResult
              True
          -- Create inner composite type with domain field
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["create type ", innerType, " as (id ", domainName, ")"]))
              mempty
              Decoders.noResult
              True
          -- Create outer composite type
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["create type ", outerType, " as (\"inner\" ", innerType, ", name text)"]))
              mempty
              Decoders.noResult
              True
          -- Test nested decoding
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select row (row (99), 'nested') :: ", outerType]))
              mempty
              ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          ( Decoders.composite
                              Nothing
                              outerType
                              ( (,)
                                  <$> Decoders.field
                                    ( Decoders.nonNullable
                                        ( Decoders.composite
                                            Nothing
                                            innerType
                                            ( Decoders.field
                                                ( Decoders.nonNullable
                                                    (Decoders.domain Nothing domainName Decoders.int8)
                                                )
                                            )
                                        )
                                    )
                                  <*> Decoders.field (Decoders.nonNullable Decoders.text)
                              )
                          )
                      )
                  )
              )
              True
        result `shouldBe` Right (99 :: Int64, "nested")

    it "fails when using plain codec for domain field in composite" \config -> do
      domainName <- Scripts.generateSymname
      compositeName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create domain type
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["create domain ", domainName, " as int8 check (value > 0)"]))
              mempty
              Decoders.noResult
              True
          -- Create composite type with domain field
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["create type ", compositeName, " as (id ", domainName, ", name text)"]))
              mempty
              Decoders.noResult
              True
          -- Try to decode composite with domain field using plain int8 decoder (should fail)
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select row (42, 'test') :: ", compositeName]))
              mempty
              ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          ( Decoders.composite
                              Nothing
                              compositeName
                              ( (,)
                                  <$> Decoders.field (Decoders.nonNullable Decoders.int8)
                                  <*> Decoders.field (Decoders.nonNullable Decoders.text)
                              )
                          )
                      )
                  )
              )
              True
        -- Should fail during field decoding
        case result of
          Left (Errors.StatementSessionError _ _ _ _ _ (Errors.UnexpectedColumnTypeStatementError 0 _ _)) ->
            pure ()
          Left err ->
            expectationFailure ("Unexpected type of error: " <> show err)
          Right _ ->
            expectationFailure "Expected an error but got success"

  describe "OID compatibility checking" do
    it "correctly validates matching domain type OIDs" \config -> do
      domainName <- Scripts.generateSymname
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          -- Create domain type
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["create domain ", domainName, " as int8 check (value > 0)"]))
              mempty
              Decoders.noResult
              True
          -- Decode with correct type - should succeed
          Session.statement ()
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select 42 :: ", domainName]))
              mempty
              ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          (Decoders.domain Nothing domainName Decoders.int8)
                      )
                  )
              )
              True
        result `shouldBe` Right (42 :: Int64)

    it "detects attempts to decode non-existent domain types" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          Session.statement ()
            $ Statement.Statement
              "select 42::int8"
              mempty
              ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          (Decoders.domain Nothing "nonexistent_domain_type" Decoders.int8)
                      )
                  )
              )
              True

        case result of
          Left (Errors.MissingTypesSessionError missingTypes) ->
            missingTypes `shouldBe` HashSet.fromList [(Nothing, "nonexistent_domain_type")]
          _ ->
            expectationFailure ("Unexpected result: " <> show result)
