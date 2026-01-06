module Pure.ByUnit.ErrorsSpec (spec) where

import Data.HashSet qualified as HashSet
import Hasql.Errors qualified as Errors
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  describe "ConnectionError" do
    describe "toMessage" do
      it "renders NetworkingConnectionError" do
        (Errors.toMessage (Errors.NetworkingConnectionError "timeout"))
          `shouldBe` "Networking error while connecting to the database"

      it "renders AuthenticationConnectionError" do
        (Errors.toMessage (Errors.AuthenticationConnectionError "invalid password"))
          `shouldBe` "Authentication error while connecting to the database"

    describe "toDetails" do
      it "includes reason for NetworkingConnectionError" do
        (Errors.toDetails (Errors.NetworkingConnectionError "connection timeout"))
          `shouldBe` [("reason", "connection timeout")]

    describe "isTransient" do
      it "NetworkingConnectionError is transient" do
        (Errors.isTransient (Errors.NetworkingConnectionError "timeout"))
          `shouldBe` True

      it "AuthenticationConnectionError is not transient" do
        (Errors.isTransient (Errors.AuthenticationConnectionError "invalid password"))
          `shouldBe` False

    describe "toDetailedText" do
      it "renders NetworkingConnectionError with details" do
        (Errors.toDetailedText (Errors.NetworkingConnectionError "connection refused"))
          `shouldBe` "Networking error while connecting to the database\n\
                     \  reason: connection refused"

  describe "ServerError" do
    describe "toMessage" do
      it "renders ServerError" do
        (Errors.toMessage (Errors.ServerError "42P01" "relation does not exist" Nothing Nothing Nothing))
          `shouldBe` "Server error"

    describe "toDetails" do
      it "includes all fields when provided" do
        (Errors.toDetails (Errors.ServerError "42P01" "relation \"users\" does not exist" (Just "The relation users does not exist.") (Just "Check your table name.") (Just 15)))
          `shouldBe` [ ("code", "42P01"),
                       ("message", "relation \"users\" does not exist"),
                       ("detail", "The relation users does not exist."),
                       ("hint", "Check your table name."),
                       ("position", "15")
                     ]

      it "excludes optional fields when not provided" do
        (Errors.toDetails (Errors.ServerError "42601" "syntax error" Nothing Nothing Nothing))
          `shouldBe` [ ("code", "42601"),
                       ("message", "syntax error")
                     ]

    describe "toDetailedText" do
      it "renders ServerError with all details" do
        (Errors.toDetailedText (Errors.ServerError "42P01" "relation \"users\" does not exist" (Just "The relation users does not exist.") (Just "Check your table name.") (Just 15)))
          `shouldBe` "Server error\n\
                     \  code: 42P01\n\
                     \  message: relation \"users\" does not exist\n\
                     \  detail: The relation users does not exist.\n\
                     \  hint: Check your table name.\n\
                     \  position: 15"

  describe "CellError" do
    describe "toMessage" do
      it "renders UnexpectedNullCellError" do
        (Errors.toMessage Errors.UnexpectedNullCellError)
          `shouldBe` "Unexpected null value"

      it "renders DeserializationCellError" do
        (Errors.toMessage (Errors.DeserializationCellError "invalid integer format"))
          `shouldBe` "Failed to deserialize cell"

    describe "toDetails" do
      it "includes no details for UnexpectedNullCellError" do
        (Errors.toDetails Errors.UnexpectedNullCellError)
          `shouldBe` []

      it "includes reason for DeserializationCellError" do
        (Errors.toDetails (Errors.DeserializationCellError "expected integer, got text"))
          `shouldBe` [("reason", "expected integer, got text")]

    describe "toDetailedText" do
      it "renders DeserializationCellError with details" do
        (Errors.toDetailedText (Errors.DeserializationCellError "invalid timestamp format"))
          `shouldBe` "Failed to deserialize cell\n\
                     \  reason: invalid timestamp format"

  describe "RowError" do
    describe "toMessage" do
      it "renders CellRowError with nested message" do
        (Errors.toMessage (Errors.CellRowError 2 23 Errors.UnexpectedNullCellError))
          `shouldBe` "Unexpected null value"

      it "renders RefinementRowError" do
        (Errors.toMessage (Errors.RefinementRowError "age must be positive"))
          `shouldBe` "Refinement error"

    describe "toDetails" do
      it "includes column index, oid, and nested cell error details" do
        (Errors.toDetails (Errors.CellRowError 3 1043 (Errors.DeserializationCellError "invalid format")))
          `shouldBe` [ ("columnIndex", "3"),
                       ("oid", "1043"),
                       ("reason", "invalid format")
                     ]

    describe "toDetailedText" do
      it "renders CellRowError with all details" do
        (Errors.toDetailedText (Errors.CellRowError 2 1043 (Errors.DeserializationCellError "invalid text encoding")))
          `shouldBe` "Failed to deserialize cell\n  columnIndex: 2\n  oid: 1043\n  reason: invalid text encoding"

  describe "StatementError" do
    describe "toMessage" do
      it "renders ServerStatementError with nested message" do
        (Errors.toMessage (Errors.ServerStatementError (Errors.ServerError "42P01" "relation does not exist" Nothing Nothing Nothing)))
          `shouldBe` "Server error"

      it "renders UnexpectedRowCountStatementError" do
        (Errors.toMessage (Errors.UnexpectedRowCountStatementError 1 1 0))
          `shouldBe` "Unexpected number of rows"

      it "renders UnexpectedColumnTypeStatementError" do
        (Errors.toMessage (Errors.UnexpectedColumnTypeStatementError 1 23 1043))
          `shouldBe` "Unexpected column type"

    describe "toDetails" do
      it "includes expected and actual for UnexpectedRowCountStatementError" do
        (Errors.toDetails (Errors.UnexpectedRowCountStatementError 1 1 5))
          `shouldBe` [("expectedMin", "1"), ("expectedMax", "1"), ("actual", "5")]

      it "includes column index and oids for UnexpectedColumnTypeStatementError" do
        (Errors.toDetails (Errors.UnexpectedColumnTypeStatementError 2 23 1043))
          `shouldBe` [("columnIndex", "2"), ("expectedOid", "23"), ("actualOid", "1043")]

    describe "toDetailedText" do
      it "renders UnexpectedRowCountStatementError with details" do
        (Errors.toDetailedText (Errors.UnexpectedRowCountStatementError 1 1 0))
          `shouldBe` "Unexpected number of rows\n  expectedMin: 1\n  expectedMax: 1\n  actual: 0"

      it "renders RowStatementError with nested details" do
        (Errors.toDetailedText (Errors.RowStatementError 3 (Errors.CellRowError 1 23 Errors.UnexpectedNullCellError)))
          `shouldBe` "Unexpected null value\n  rowIndex: 3\n  columnIndex: 1\n  oid: 23"

  describe "SessionError" do
    describe "toMessage" do
      it "renders StatementSessionError with nested message" do
        (Errors.toMessage (Errors.StatementSessionError 1 0 "SELECT 1" [] True (Errors.UnexpectedRowCountStatementError 1 1 0)))
          `shouldBe` "Unexpected number of rows"

      it "renders ConnectionSessionError" do
        (Errors.toMessage (Errors.ConnectionSessionError "connection lost"))
          `shouldBe` "Connection error"

      it "renders MissingTypesSessionError" do
        (Errors.toMessage (Errors.MissingTypesSessionError (HashSet.fromList [(Just "public", "custom_type"), (Nothing, "enum_type")])))
          `shouldBe` "Types not found in database"

    describe "toDetails" do
      it "includes statement context and nested error details" do
        (Errors.toDetails (Errors.StatementSessionError 3 1 "SELECT * FROM users WHERE id = $1" ["42"] True (Errors.ServerStatementError (Errors.ServerError "42P01" "relation does not exist" Nothing Nothing Nothing))))
          `shouldBe` [ ("totalStatements", "3"),
                       ("statementIndex", "1"),
                       ("sql", "SELECT * FROM users WHERE id = $1"),
                       ("parameters", "42"),
                       ("prepared", "true"),
                       ("code", "42P01"),
                       ("message", "relation does not exist")
                     ]

      it "includes multiple parameters" do
        (Errors.toDetails (Errors.StatementSessionError 1 0 "INSERT INTO users (name, age) VALUES ($1, $2)" ["Alice", "30"] False (Errors.UnexpectedRowCountStatementError 1 1 0)))
          `shouldBe` [ ("totalStatements", "1"),
                       ("statementIndex", "0"),
                       ("sql", "INSERT INTO users (name, age) VALUES ($1, $2)"),
                       ("parameters", "Alice, 30"),
                       ("prepared", "false"),
                       ("expectedMin", "1"),
                       ("expectedMax", "1"),
                       ("actual", "0")
                     ]

    describe "isTransient" do
      it "ConnectionSessionError is transient" do
        (Errors.isTransient (Errors.ConnectionSessionError "connection lost"))
          `shouldBe` True

      it "StatementSessionError is not transient" do
        (Errors.isTransient (Errors.StatementSessionError 1 0 "SELECT 1" [] True (Errors.UnexpectedRowCountStatementError 1 1 0)))
          `shouldBe` False

    describe "toDetailedText" do
      it "renders StatementSessionError with all context" do
        (Errors.toDetailedText (Errors.StatementSessionError 1 0 "SELECT * FROM users" [] True (Errors.UnexpectedRowCountStatementError 1 1 10)))
          `shouldBe` "Unexpected number of rows\n\
                     \  totalStatements: 1\n\
                     \  statementIndex: 0\n\
                     \  sql: SELECT * FROM users\n\
                     \  parameters:\n\
                     \  prepared: true\n\
                     \  expectedMin: 1\n\
                     \  expectedMax: 1\n\
                     \  actual: 10"

  describe "toDetailedText with multiline values" do
    it "indents multiline detail values correctly" do
      (Errors.toDetailedText (Errors.ServerError "42601" "syntax error" (Just "Line 1: syntax error\nLine 2: near unexpected token\nLine 3: suggestion here") Nothing Nothing))
        `shouldBe` "Server error\n\
                   \  code: 42601\n\
                   \  message: syntax error\n\
                   \  detail:\n\
                   \    Line 1: syntax error\n\
                   \    Line 2: near unexpected token\n\
                   \    Line 3: suggestion here"

    it "indents multiline hint values correctly" do
      (Errors.toDetailedText (Errors.ServerError "42P01" "relation not found" Nothing (Just "Perhaps you meant:\n  users\n  user_accounts\n  user_profiles") Nothing))
        `shouldBe` "Server error\n\
                   \  code: 42P01\n\
                   \  message: relation not found\n\
                   \  hint:\n\
                   \    Perhaps you meant:\n\
                   \      users\n\
                   \      user_accounts\n\
                   \      user_profiles"

    it "handles multiline SQL in StatementSessionError" do
      (Errors.toDetailedText (Errors.StatementSessionError 1 0 "SELECT *\nFROM users\nWHERE id = $1" ["1"] False (Errors.UnexpectedRowCountStatementError 1 1 0)))
        `shouldBe` "Unexpected number of rows\n\
                   \  totalStatements: 1\n\
                   \  statementIndex: 0\n\
                   \  sql:\n\
                   \    SELECT *\n\
                   \    FROM users\n\
                   \    WHERE id = $1\n\
                   \  parameters: 1\n\
                   \  prepared: false\n\
                   \  expectedMin: 1\n\
                   \  expectedMax: 1\n\
                   \  actual: 0"
