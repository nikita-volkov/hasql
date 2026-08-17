module Pure.ErrorsSpec (spec) where

import Data.HashSet qualified as HashSet
import Hasql.Errors qualified as Errors
import Prelude
import Test.Hspec

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

    describe "toSqlState" do
      it "is Nothing, since connection errors carry no server code" do
        (Errors.toSqlState (Errors.NetworkingConnectionError "timeout"))
          `shouldBe` Nothing

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

    describe "isTransient" do
      it "42P05 (prepared statement already exists) is transient" do
        (Errors.isTransient (Errors.ServerError "42P05" "prepared statement \"hasql_x\" already exists" Nothing Nothing Nothing))
          `shouldBe` True

      it "other codes are not transient" do
        (Errors.isTransient (Errors.ServerError "42P01" "relation does not exist" Nothing Nothing Nothing))
          `shouldBe` False

    describe "toSqlState" do
      it "is the code the server reported" do
        (Errors.toSqlState (Errors.ServerError "23505" "duplicate key value violates unique constraint" Nothing Nothing Nothing))
          `shouldBe` Just "23505"

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

    describe "isTransient" do
      it "delegates to the wrapped ServerError for 42P05" do
        (Errors.isTransient (Errors.ServerStatementError (Errors.ServerError "42P05" "prepared statement \"hasql_x\" already exists" Nothing Nothing Nothing)))
          `shouldBe` True

      it "delegates to the wrapped ServerError for other codes" do
        (Errors.isTransient (Errors.ServerStatementError (Errors.ServerError "42P01" "relation does not exist" Nothing Nothing Nothing)))
          `shouldBe` False

      it "is not transient for a row count mismatch" do
        (Errors.isTransient (Errors.UnexpectedRowCountStatementError 1 1 0))
          `shouldBe` False

    describe "toSqlState" do
      it "digs the code out of ServerStatementError" do
        (Errors.toSqlState (Errors.ServerStatementError (Errors.ServerError "23505" "duplicate key" Nothing Nothing Nothing)))
          `shouldBe` Just "23505"

      it "is Nothing for a decoding failure" do
        (Errors.toSqlState (Errors.RowStatementError 3 (Errors.CellRowError 1 23 Errors.UnexpectedNullCellError)))
          `shouldBe` Nothing

      it "is Nothing for a row count mismatch" do
        (Errors.toSqlState (Errors.UnexpectedRowCountStatementError 1 1 0))
          `shouldBe` Nothing

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

      it "renders ClientRejectionSessionError" do
        (Errors.toMessage (Errors.ClientRejectionSessionError "cannot handle more than 65535 parameters"))
          `shouldBe` "Client rejected the request"

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

      it "ClientRejectionSessionError is not transient, since retrying sends the same rejected request again" do
        (Errors.isTransient (Errors.ClientRejectionSessionError "cannot handle more than 65535 parameters"))
          `shouldBe` False

      it "StatementSessionError is not transient for a non-transient statement error" do
        (Errors.isTransient (Errors.StatementSessionError 1 0 "SELECT 1" [] True (Errors.UnexpectedRowCountStatementError 1 1 0)))
          `shouldBe` False

      it "StatementSessionError is transient when the wrapped statement error is a 42P05" do
        (Errors.isTransient (Errors.StatementSessionError 1 0 "SELECT 1" [] True (Errors.ServerStatementError (Errors.ServerError "42P05" "prepared statement \"hasql_x\" already exists" Nothing Nothing Nothing))))
          `shouldBe` True

      it "ScriptSessionError delegates to the wrapped ServerError" do
        (Errors.isTransient (Errors.ScriptSessionError "select 1" (Errors.ServerError "42P05" "prepared statement \"hasql_x\" already exists" Nothing Nothing Nothing)))
          `shouldBe` True

    describe "toSqlState" do
      it "digs the code out of StatementSessionError" do
        (Errors.toSqlState (Errors.StatementSessionError 1 0 "INSERT INTO users (email) VALUES ($1)" ["a@b.c"] True (Errors.ServerStatementError (Errors.ServerError "23505" "duplicate key" Nothing Nothing Nothing))))
          `shouldBe` Just "23505"

      it "digs the code out of ScriptSessionError" do
        (Errors.toSqlState (Errors.ScriptSessionError "DROP TABLE users" (Errors.ServerError "42P01" "relation does not exist" Nothing Nothing Nothing)))
          `shouldBe` Just "42P01"

      it "is Nothing for StatementSessionError wrapping a non-server error" do
        (Errors.toSqlState (Errors.StatementSessionError 1 0 "SELECT 1" [] True (Errors.UnexpectedRowCountStatementError 1 1 0)))
          `shouldBe` Nothing

      it "is Nothing for ConnectionSessionError" do
        (Errors.toSqlState (Errors.ConnectionSessionError "connection lost"))
          `shouldBe` Nothing

      it "is Nothing for ClientRejectionSessionError" do
        (Errors.toSqlState (Errors.ClientRejectionSessionError "cannot handle more than 65535 parameters"))
          `shouldBe` Nothing

      it "is Nothing for MissingTypesSessionError" do
        (Errors.toSqlState (Errors.MissingTypesSessionError (HashSet.fromList [(Just "public", "custom_type")])))
          `shouldBe` Nothing

      it "is Nothing for DriverSessionError" do
        (Errors.toSqlState (Errors.DriverSessionError "unexpected response"))
          `shouldBe` Nothing

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
