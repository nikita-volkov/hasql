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

    -- * Connection errors
    ConnectionError (..),

    -- * Session errors
    SessionError (..),
    StatementError (..),
    CellError (..),
    ExecutionError (..),
  )
where

import Data.Text qualified as Text
import Hasql.Engine.Errors
import Hasql.Platform.Prelude
import TextBuilder qualified
import Hasql.TextExtras qualified as TextExtras

-- * Classes

-- | A class for types that can be treated as errors.
class IsError a where
  -- | Convert the error to a human-readable message.
  toErrorMessage :: a -> Text

  -- | Whether the error is transient and the operation causing it can be retried.
  isTransient :: a -> Bool

-- * Instances

instance IsError ConnectionError where
  toErrorMessage = \case
    NetworkingConnectionError details ->
      mconcat
        [ "Networking error while connecting to the database: ",
          details
        ]
    AuthenticationConnectionError details ->
      mconcat
        [ "Authentication error while connecting to the database: ",
          details
        ]
    CompatibilityConnectionError details ->
      mconcat
        [ "Compatibility error while connecting to the database: ",
          details
        ]
    OtherConnectionError details ->
      mconcat
        [ "Connection error while connecting to the database: ",
          details
        ]

  isTransient = \case
    NetworkingConnectionError {} -> True
    AuthenticationConnectionError {} -> False
    CompatibilityConnectionError {} -> False
    OtherConnectionError {} -> False

instance IsError ExecutionError where
  toErrorMessage (ExecutionError code message detail hint position) =
    (TextBuilder.toText . mconcat . mconcat)
      [ [ TextBuilder.text code,
          " - ",
          TextBuilder.text message
        ],
        maybe [] (\d -> [" Detail: " <> TextBuilder.text d]) detail,
        maybe [] (\h -> [" Hint: " <> TextBuilder.text h]) hint,
        maybe [] (\p -> [" Position: " <> TextBuilder.decimal p]) position
      ]
  isTransient = const False

instance IsError CellError where
  toErrorMessage = \case
    UnexpectedNullCellError ->
      "Unexpected null value"
    DeserializationCellError message ->
      mconcat
        [ "Failed to deserialize cell: ",
          message
        ]
  isTransient = const False

instance IsError StatementError where
  toErrorMessage = \case
    ExecutionStatementError executionError ->
      mconcat
        [ "Server error: ",
          toErrorMessage executionError
        ]
    UnexpectedRowCountError min max actual ->
      (TextBuilder.toText . mconcat)
        [ "Unexpected number of rows: expected ",
          if min == max
            then TextBuilder.decimal min
            else TextBuilder.decimal min <> " to " <> TextBuilder.decimal max,
          ", got ",
          TextBuilder.decimal actual
        ]
    UnexpectedAmountOfColumnsStatementError expected actual ->
      (TextBuilder.toText . mconcat)
        [ "Unexpected number of columns: expected ",
          TextBuilder.decimal expected,
          ", got ",
          TextBuilder.decimal actual
        ]
    UnexpectedColumnTypeStatementError colIdx expected actual ->
      (TextBuilder.toText . mconcat)
        [ "Unexpected column type at index ",
          TextBuilder.decimal colIdx,
          ": expected OID ",
          TextBuilder.decimal expected,
          ", got OID ",
          TextBuilder.decimal actual
        ]
    CellStatementError rowIdx colIdx cellErr ->
      (TextBuilder.toText . mconcat)
        [ "In row ",
          TextBuilder.decimal rowIdx,
          ", column ",
          TextBuilder.decimal colIdx,
          ": ",
          TextBuilder.text (toErrorMessage cellErr)
        ]
    UnexpectedResultStatementError message ->
      mconcat
        [ "Driver error: ",
          message
        ]
  isTransient = const False

instance IsError SessionError where
  toErrorMessage = \case
    StatementSessionError totalStatements statementIndex sql parameters prepared statementError ->
      (TextBuilder.toText . mconcat)
        [ "In ",
          if prepared then "prepared" else "unprepared",
          " statement at offset ",
          TextBuilder.decimal statementIndex,
          " of pipeline with ",
          TextBuilder.decimal totalStatements,
          " statements.\n  SQL:\n    ",
          sql
            & decodeUtf8Lenient
            & TextExtras.prefixEachLine "    "
            & TextBuilder.text,
          "\n  Params:\n    ",
          parameters
            & fmap (mappend "- " . TextExtras.prefixEachLine "  ")
            & Text.intercalate "\n"
            & TextExtras.prefixEachLine "    "
            & TextBuilder.text,
          "\n  Error: ",
          TextBuilder.text (toErrorMessage statementError)
        ]
    ScriptSessionError sql execErr ->
      mconcat
        [ "In script.\n  SQL:\n    ",
          TextExtras.prefixEachLine "    " (decodeUtf8Lenient sql),
          "\n  Error: ",
          toErrorMessage execErr
        ]
    ConnectionSessionError message ->
      mconcat
        [ "Connection error: ",
          message
        ]
    DriverSessionError message ->
      mconcat
        [ "Driver error: ",
          message
        ]
    MissingTypesSessionError missingTypes ->
      (TextBuilder.toText . mconcat)
        [ "The following types were not found in the database:\n",
          missingTypes
            & toList
            & fmap
              ( \(schema, name) ->
                  "  - "
                    <> maybe "" (<> ".") schema
                    <> name
              )
            & mconcat
            . intersperse "\n"
            & TextBuilder.text
        ]

  isTransient = \case
    ConnectionSessionError _ -> True
    StatementSessionError {} -> False
    ScriptSessionError {} -> False
    DriverSessionError {} -> False
    MissingTypesSessionError {} -> False
