-- |
-- Explicit error types for all Hasql operations.
--
-- This module provides access to all error types used throughout Hasql:
--
-- * 'AcquireError' - errors that occur when establishing a database connection
-- * 'UseError' - errors returned when using a connection
-- * 'SessionError' - recoverable errors that occur during session execution
--
-- The module follows Hasql's philosophy of explicit error handling,
-- where all errors are represented as values rather than exceptions.
module Hasql.Errors
  ( -- * Error class
    IsError (..),
    toDetailedText,

    -- * Acquire errors
    AcquireError (..),

    -- * Use errors
    UseError (..),

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
--
-- This is a rendering interface: it turns an error value into a
-- human-readable message, a list of dynamic details, and - where the error
-- carries one - the server's SQLSTATE. It deliberately does not offer a
-- retryability verdict. Whether an operation is worth retrying depends on
-- the caller's retry policy, the transaction it sits inside, and the
-- SQLSTATE where one is available - not on a boolean this class hands out.
-- Hasql declines to own that decision, so the omission here is not an
-- oversight.
class IsError a where
  -- | Convert the error to a human-readable message with no dynamic details.
  toMessage :: a -> Text

  -- | Convert the error to a list of key-value pairs of dynamic details.
  toDetails :: a -> [(Text, Text)]

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

instance IsError AcquireError where
  toMessage = \case
    NetworkingAcquireError {} ->
      "Networking error while connecting to the database"
    AuthenticationAcquireError {} ->
      "Authentication error while connecting to the database"
    CompatibilityAcquireError {} ->
      "Compatibility error while connecting to the database"
    OtherAcquireError {} ->
      "Connection error while connecting to the database"

  toDetails = \case
    NetworkingAcquireError reason ->
      [("reason", reason)]
    AuthenticationAcquireError reason ->
      [("reason", reason)]
    CompatibilityAcquireError reason ->
      [("reason", reason)]
    OtherAcquireError reason ->
      [("reason", reason)]

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

instance IsError SessionError where
  toMessage = \case
    StatementSessionError _ _ _ _ _ statementError ->
      toMessage statementError
    ScriptSessionError _ execErr ->
      toMessage execErr
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
    MissingTypesSessionError missingTypes ->
      [ ( "missingTypes",
          (TextBuilder.toText . mconcat . intersperse ", " . fmap formatType . HashSet.toList) missingTypes
        )
      ]
      where
        formatType (schema, name) = maybe "" ((<> ".") . TextBuilder.text) schema <> TextBuilder.text name

  toSqlState = \case
    StatementSessionError _ _ _ _ _ statementError -> toSqlState statementError
    ScriptSessionError _ serverError -> toSqlState serverError
    MissingTypesSessionError {} -> Nothing

instance IsError UseError where
  toMessage = \case
    SessionUseError err ->
      toMessage err
    ConnectionUseError {} ->
      "Connection error"

  toDetails = \case
    SessionUseError err ->
      toDetails err
    ConnectionUseError reason ->
      [("reason", reason)]

  toSqlState = \case
    SessionUseError err -> toSqlState err
    ConnectionUseError {} -> Nothing
