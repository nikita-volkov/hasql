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

  -- | Whether the error is transient and the operation causing it can be retried.
  isTransient :: a -> Bool

-- | Convert the error to a multiline detailed human-readable text representation containing all details.
toDetailedText :: (IsError e) => e -> Text
toDetailedText err =
  TextBuilder.toText
    ( TextBuilder.text (toMessage err)
        <> foldMap
          ( \(key, value) ->
              mconcat
                [ "\n  ",
                  TextBuilder.text key,
                  ": ",
                  TextBuilder.intercalateMap
                    "\n    "
                    TextBuilder.text
                    (Text.lines value)
                ]
          )
          (toDetails err)
    )

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

  isTransient = const False

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
    UnexpectedAmountOfColumnsStatementError {} ->
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
    UnexpectedAmountOfColumnsStatementError expected actual ->
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

  isTransient = const False

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
    StatementSessionError {} -> False
    ScriptSessionError {} -> False
    DriverSessionError {} -> False
    MissingTypesSessionError {} -> False
