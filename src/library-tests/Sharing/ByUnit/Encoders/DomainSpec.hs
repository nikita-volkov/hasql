module Sharing.ByUnit.Encoders.DomainSpec (spec) where

import Data.HashSet qualified as HashSet
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
  describe "Simple domains" do
    it "encodes a simple domain and compares with static value" \config -> do
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
          -- Test encoding by comparing with static value
          Session.statement (42 :: Int64)
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select ($1 :: ", domainName, ") = (42 :: ", domainName, ")"]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      (Encoders.domain Nothing domainName Encoders.int8)
                  )
              )
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
              True
        result `shouldBe` Right True

    it "encodes and roundtrips a simple domain" \config -> do
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
          -- Test roundtrip
          Session.statement (42 :: Int64)
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select $1 :: ", domainName]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      (Encoders.domain Nothing domainName Encoders.int8)
                  )
              )
              ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          (Decoders.domain Nothing domainName Decoders.int8)
                      )
                  )
              )
              True
        result `shouldBe` Right (42 :: Int64)

    it "encodes a text domain" \config -> do
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
          -- Test roundtrip
          Session.statement "hello world"
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select $1 :: ", domainName]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      (Encoders.domain Nothing domainName Encoders.text)
                  )
              )
              ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          (Decoders.domain Nothing domainName Decoders.text)
                      )
                  )
              )
              True
        result `shouldBe` Right "hello world"

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
    it "encodes arrays of domain values" \config -> do
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
          -- Test array encoding
          Session.statement [1 :: Int32, 2, 3, 4, 5]
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select $1 = array[1, 2, 3, 4, 5] :: ", domainName, "[]"]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.array
                          ( Encoders.dimension
                              foldl'
                              ( Encoders.element
                                  ( Encoders.nonNullable
                                      (Encoders.domain Nothing domainName Encoders.int4)
                                  )
                              )
                          )
                      )
                  )
              )
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
              True
        result `shouldBe` Right True

    it "roundtrips arrays of domain values" \config -> do
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
          -- Test roundtrip
          Session.statement [1 :: Int32, 2, 3, 4, 5]
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select $1 :: ", domainName, "[]"]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.array
                          ( Encoders.dimension
                              foldl'
                              ( Encoders.element
                                  ( Encoders.nonNullable
                                      (Encoders.domain Nothing domainName Encoders.int4)
                                  )
                              )
                          )
                      )
                  )
              )
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
    it "encodes composites with domain fields" \config -> do
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
          -- Test encoding
          Session.statement (42 :: Int64, "test")
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select ($1 :: ", compositeName, ") = row (42, 'test') :: ", compositeName]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.composite
                          Nothing
                          compositeName
                          ( divide
                              (\(a, b) -> (a, b))
                              ( Encoders.field
                                  ( Encoders.nonNullable
                                      (Encoders.domain Nothing domainName Encoders.int8)
                                  )
                              )
                              (Encoders.field (Encoders.nonNullable Encoders.text))
                          )
                      )
                  )
              )
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
              True
        result `shouldBe` Right True

    it "roundtrips composites with domain fields" \config -> do
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
          -- Test roundtrip
          Session.statement (42 :: Int64, "test")
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select $1 :: ", compositeName]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      ( Encoders.composite
                          Nothing
                          compositeName
                          ( divide
                              (\(a, b) -> (a, b))
                              ( Encoders.field
                                  ( Encoders.nonNullable
                                      (Encoders.domain Nothing domainName Encoders.int8)
                                  )
                              )
                              (Encoders.field (Encoders.nonNullable Encoders.text))
                          )
                      )
                  )
              )
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

  describe "OID lookup verification" do
    it "requests OID for domains (verified by successful execution)" \config -> do
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
          -- Use domain - this requires OID lookup to succeed
          Session.statement (100 :: Int64)
            $ Statement.Statement
              (encodeUtf8 (mconcat ["select $1 :: ", domainName]))
              ( Encoders.param
                  ( Encoders.nonNullable
                      (Encoders.domain Nothing domainName Encoders.int8)
                  )
              )
              ( Decoders.singleRow
                  ( Decoders.column
                      ( Decoders.nonNullable
                          (Decoders.domain Nothing domainName Decoders.int8)
                      )
                  )
              )
              True
        result `shouldBe` Right (100 :: Int64)

    it "detects attempts to encode non-existent domain types" \config -> do
      Scripts.onPreparableConnection config \connection -> do
        result <- Connection.use connection do
          Session.statement (42 :: Int64)
            $ Statement.Statement
              "select $1::nonexistent_domain_type"
              ( Encoders.param
                  ( Encoders.nonNullable
                      (Encoders.domain Nothing "nonexistent_domain_type" Encoders.int8)
                  )
              )
              (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
              True

        case result of
          Left (Errors.MissingTypesSessionError missingTypes) ->
            missingTypes `shouldBe` HashSet.fromList [(Nothing, "nonexistent_domain_type")]
          _ ->
            expectationFailure ("Unexpected result: " <> show result)
