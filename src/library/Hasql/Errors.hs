-- |
-- Explicit error types for all Hasql operations.
--
-- This module provides access to all error types used throughout Hasql:
--
-- * 'ConnectionError' - errors that occur when establishing a database connection
-- * 'SessionError' - errors that occur during session execution
--
-- The module follows Hasql's philosophy of explicit error handling,
-- where all errors are represented as values rather than exceptions.
module Hasql.Errors
  ( -- * Error class
    IsError (..),
    toDetailedText,

    -- * Connection errors
    ConnectionError (..),

    -- * Session errors
    SessionError (..),
    StatementError (..),
    RowError (..),
    CellError (..),
    ServerError (..),
  )
where

import Data.HashSet qualified as HashSet
import Data.Text qualified as Text
import Hasql.Engine.Errors
import Hasql.Platform.Prelude
import TextBuilder qualified

-- * Classes

-- | A class for types that can be treated as errors.
class IsError a where
  -- | Convert the error to a human-readable message with no dynamic details.
  toMessage :: a -> Text

  -- | Convert the error to a list of key-value pairs of dynamic details.
  toDetails :: a -> [(Text, Text)]

  -- | Whether retrying against a clean connection state will succeed.
  --
  -- This is a statement about the connection, not about the caller's unit of
  -- work. Inside an explicit transaction a failed statement cannot be
  -- retried in place: the transaction is aborted and only a @ROLLBACK@ gets
  -- the connection out of that state, same as for any other error. So the
  -- unit that's safe to retry is the caller's whole transaction (or
  -- pipeline), not necessarily the single statement that reported 'True'
  -- here.
  --
  -- Note what supplying a clean connection state takes for the errors that
  -- say the connection is gone. 'ConnectionSessionError' is transient, but
  -- 'Hasql.Connection.use' has finished the connection before returning it,
  -- and the 'Hasql.Connection.Connection' it fired on never carries a clean
  -- state again: every later 'Hasql.Connection.use' on that same handle
  -- reports the same transient error. Retrying there means acquiring a
  -- connection or asking a pool for one, never calling
  -- 'Hasql.Connection.use' again on the value that just failed. A loop that
  -- retries in place on a 'True' from this method does not eventually
  -- recover; it spins.
  isTransient :: a -> Bool

  -- | The SQLSTATE the server reported, if this error carries one at all.
  --
  -- Lets you branch on a PostgreSQL error code without knowing which
  -- constructors of which error type the server error is nested under. For the
  -- code vocabulary see
  -- <https://www.postgresql.org/docs/current/errcodes-appendix.html>.
  --
  -- 'Nothing' means the error carries no server code: a connection failure, a
  -- decoding failure, a driver bug. It never means "the operation succeeded".
  --
  -- The default implementation returns 'Nothing', which is correct only for
  -- error types that can never carry a server error. A type that wraps another
  -- error type MUST override it and delegate to the wrapped value, otherwise it
  -- silently reports 'Nothing' for codes it does in fact carry.
  toSqlState :: a -> Maybe Text
  toSqlState _ = Nothing

-- | Convert the error to a multiline detailed human-readable text representation containing all details.
toDetailedText :: (IsError e) => e -> Text
toDetailedText = TextBuilder.toText . toDetailedTextBuilder

-- | Convert the error to a multiline detailed human-readable text representation containing all details.
toDetailedTextBuilder :: (IsError e) => e -> TextBuilder
toDetailedTextBuilder err =
  TextBuilder.text (toMessage err)
    <> foldMap
      ( \(key, value) ->
          mconcat
            [ "\n  ",
              TextBuilder.text key,
              case Text.lines value of
                [] -> ":"
                [singleLine] ->
                  ": " <> TextBuilder.text singleLine
                multipleLines ->
                  ":" <> foldMap (mappend "\n    " . TextBuilder.text) multipleLines
            ]
      )
      (toDetails err)

-- * Instances

instance IsError ConnectionError where
  toMessage = \case
    NetworkingConnectionError {} ->
      "Networking error while connecting to the database"
    AuthenticationConnectionError {} ->
      "Authentication error while connecting to the database"
    CompatibilityConnectionError {} ->
      "Compatibility error while connecting to the database"
    OtherConnectionError {} ->
      "Connection error while connecting to the database"

  toDetails = \case
    NetworkingConnectionError reason ->
      [("reason", reason)]
    AuthenticationConnectionError reason ->
      [("reason", reason)]
    CompatibilityConnectionError reason ->
      [("reason", reason)]
    OtherConnectionError reason ->
      [("reason", reason)]

  isTransient = \case
    NetworkingConnectionError {} -> True
    AuthenticationConnectionError {} -> False
    CompatibilityConnectionError {} -> False
    OtherConnectionError {} -> False

instance IsError ServerError where
  toMessage _ =
    "Server error"

  toDetails (ServerError code message detail hint position) =
    mconcat
      [ [("code", code), ("message", message)],
        maybe [] (\d -> [("detail", d)]) detail,
        maybe [] (\h -> [("hint", h)]) hint,
        maybe [] (\p -> [("position", (TextBuilder.toText . TextBuilder.decimal) p)]) position
      ]

  -- 42P05 ("prepared statement already exists") is transient: prepared
  -- statement names are content-addressed, so a collision on the name is a
  -- collision on the statement, and the driver keeps the cache mapping that
  -- lets the next use find it warm.
  --
  -- The rest are the SQLSTATEs that a retry against a clean connection can
  -- plausibly turn into success: serialization/deadlock conflicts, lock
  -- timeouts, the server shutting the connection down or refusing new work,
  -- resource exhaustion, and landing on a standby after failover.
  isTransient (ServerError code _ _ _ _) =
    code
      `elem` [ "42P05", -- prepared_statement_exists
               "40001", -- serialization_failure
               "40P01", -- deadlock_detected
               "55P03", -- lock_not_available
               "57P01", -- admin_shutdown
               "57P02", -- crash_shutdown
               "57P03", -- cannot_connect_now
               "08000", -- connection_exception
               "08003", -- connection_does_not_exist
               "08006", -- connection_failure
               "53100", -- disk_full
               "53200", -- out_of_memory
               "53300", -- too_many_connections
               "25006" -- read_only_sql_transaction
             ]

  toSqlState (ServerError code _ _ _ _) = Just code

instance IsError CellError where
  toMessage = \case
    UnexpectedNullCellError ->
      "Unexpected null value"
    DeserializationCellError {} ->
      "Failed to deserialize cell"

  toDetails = \case
    UnexpectedNullCellError ->
      []
    DeserializationCellError reason ->
      [("reason", reason)]

  isTransient = const False

instance IsError StatementError where
  toMessage = \case
    ServerStatementError executionError ->
      toMessage executionError
    UnexpectedRowCountStatementError {} ->
      "Unexpected number of rows"
    UnexpectedColumnCountStatementError {} ->
      "Unexpected number of columns"
    UnexpectedColumnTypeStatementError {} ->
      "Unexpected column type"
    RowStatementError _ rowError ->
      toMessage rowError
    UnexpectedResultStatementError {} ->
      "Driver error"

  toDetails = \case
    ServerStatementError executionError ->
      toDetails executionError
    UnexpectedRowCountStatementError min max actual ->
      [ ("expectedMin", (TextBuilder.toText . TextBuilder.decimal) min),
        ("expectedMax", (TextBuilder.toText . TextBuilder.decimal) max),
        ("actual", (TextBuilder.toText . TextBuilder.decimal) actual)
      ]
    UnexpectedColumnCountStatementError expected actual ->
      [ ("expected", (TextBuilder.toText . TextBuilder.decimal) expected),
        ("actual", (TextBuilder.toText . TextBuilder.decimal) actual)
      ]
    UnexpectedColumnTypeStatementError colIdx expected actual ->
      [ ("columnIndex", (TextBuilder.toText . TextBuilder.decimal) colIdx),
        ("expectedOid", (TextBuilder.toText . TextBuilder.decimal) expected),
        ("actualOid", (TextBuilder.toText . TextBuilder.decimal) actual)
      ]
    RowStatementError rowIdx rowError ->
      ("rowIndex", (TextBuilder.toText . TextBuilder.decimal) rowIdx) : toDetails rowError
    UnexpectedResultStatementError reason ->
      [("reason", reason)]

  isTransient = \case
    ServerStatementError executionError -> isTransient executionError
    UnexpectedRowCountStatementError {} -> False
    UnexpectedColumnCountStatementError {} -> False
    UnexpectedColumnTypeStatementError {} -> False
    RowStatementError _ rowError -> isTransient rowError
    UnexpectedResultStatementError {} -> False

  toSqlState = \case
    ServerStatementError executionError -> toSqlState executionError
    UnexpectedRowCountStatementError {} -> Nothing
    UnexpectedColumnCountStatementError {} -> Nothing
    UnexpectedColumnTypeStatementError {} -> Nothing
    RowStatementError _ rowError -> toSqlState rowError
    UnexpectedResultStatementError {} -> Nothing

instance IsError RowError where
  toMessage = \case
    CellRowError _ _ cellErr ->
      toMessage cellErr
    RefinementRowError {} ->
      "Refinement error"

  toDetails = \case
    CellRowError colIdx oid cellErr ->
      [ ("columnIndex", (TextBuilder.toText . TextBuilder.decimal) colIdx),
        ("oid", (TextBuilder.toText . TextBuilder.decimal) oid)
      ]
        <> toDetails cellErr
    RefinementRowError reason ->
      [("reason", reason)]

  isTransient = const False

instance IsError SessionError where
  toMessage = \case
    StatementSessionError _ _ _ _ _ statementError ->
      toMessage statementError
    ScriptSessionError _ execErr ->
      toMessage execErr
    ConnectionSessionError {} ->
      "Connection error"
    DriverSessionError {} ->
      "Driver error"
    MissingTypesSessionError {} ->
      "Types not found in database"

  toDetails = \case
    StatementSessionError totalStatements statementIndex sql parameters prepared statementError ->
      [ ("totalStatements", (TextBuilder.toText . TextBuilder.decimal) totalStatements),
        ("statementIndex", (TextBuilder.toText . TextBuilder.decimal) statementIndex),
        ("sql", sql),
        ("parameters", Text.intercalate ", " parameters),
        ("prepared", if prepared then "true" else "false")
      ]
        <> toDetails statementError
    ScriptSessionError sql execErr ->
      ("sql", sql) : toDetails execErr
    ConnectionSessionError reason ->
      [("reason", reason)]
    DriverSessionError reason ->
      [("reason", reason)]
    MissingTypesSessionError missingTypes ->
      [ ( "missingTypes",
          (TextBuilder.toText . mconcat . intersperse ", " . fmap formatType . HashSet.toList) missingTypes
        )
      ]
      where
        formatType (schema, name) = maybe "" ((<> ".") . TextBuilder.text) schema <> TextBuilder.text name

  isTransient = \case
    ConnectionSessionError _ -> True
    StatementSessionError _ _ _ _ _ statementError -> isTransient statementError
    ScriptSessionError _ serverError -> isTransient serverError
    DriverSessionError {} -> False
    MissingTypesSessionError {} -> False

  toSqlState = \case
    StatementSessionError _ _ _ _ _ statementError -> toSqlState statementError
    ScriptSessionError _ serverError -> toSqlState serverError
    ConnectionSessionError {} -> Nothing
    DriverSessionError {} -> Nothing
    MissingTypesSessionError {} -> Nothing
