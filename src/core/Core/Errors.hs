module Core.Errors where

import Hipq.Recv qualified
import Hipq.ResultDecoder qualified
import Hipq.ResultRowDecoder qualified
import Platform.Prelude
import Pq qualified
import TextBuilder qualified
import TextBuilderExtras qualified

-- * Error types

-- |
-- Connection acquisition error.
data ConnectionError
  = NetworkingConnectionError
      -- | Human readable details indended for logging.
      Text
  | AuthenticationConnectionError
      -- | Human readable details indended for logging.
      Text
  | CompatibilityConnectionError
      -- | Human readable details indended for logging.
      Text
  | -- | Uncategorized error coming from "libpq". May be empty text.
    OtherConnectionError
      -- | Human readable details intended for logging.
      Text
  deriving stock (Show, Eq)

-- |
-- Execution error from PostgreSQL server.
data ExecutionError
  = ExecutionError
      -- | Code.
      Text
      -- | Message.
      Text
      -- | Detail.
      (Maybe Text)
      -- | Hint.
      (Maybe Text)
      -- | Position (1-based index in the SQL string).
      (Maybe Int)
  deriving stock (Show, Eq)

-- |
-- Cell-level decoding error.
data CellError
  = UnexpectedNullCellError
  | CellDeserializationError
      -- | Type OID.
      Word32
      -- | Underlying error.
      Text
  deriving stock (Show, Eq)

-- |
-- Statement-level error.
data StatementError
  = ExecutionStatementError ExecutionError
  | UnexpectedResultStatementError
      -- | Details.
      Text
  | RowCountStatementError
      -- | Expected minimum.
      Int
      -- | Expected maximum.
      Int
      -- | Actual.
      Int
  | UnexpectedAmountOfColumnsStatementError
      -- | Expected count.
      Int
      -- | Actual count.
      Int
  | CellStatementError
      -- | 0-based row index.
      Int
      -- | 0-based column index.
      Int
      -- | Underlying cell error.
      CellError
  deriving stock (Show, Eq)

data SessionError
  = StatementSessionError
      -- | Total number of statements in the pipeline.
      Int
      -- | 0-based index of the statement in the pipeline.
      Int
      -- | SQL template.
      ByteString
      -- | Parameters.
      [Text]
      -- | Whether the statement was executed as a prepared statement.
      Bool
      -- | Underlying statement error.
      StatementError
  | ScriptSessionError
      -- | SQL.
      ByteString
      -- | Server error.
      ExecutionError
  | ConnectionSessionError
      Text
  | -- | Either the server misbehaves or there is a bug in Hasql.
    DriverSessionError
      Text
  deriving stock (Show, Eq)

fromRecvError :: Hipq.Recv.Error (Maybe (Int, Int, ByteString, [Text], Bool)) -> SessionError
fromRecvError = \case
  Hipq.Recv.ResultError location _resultOffset resultError ->
    case location of
      Nothing ->
        (DriverSessionError . TextBuilder.toText . mconcat)
          [ "Unexpected error outside of statement context. ",
            "This indicates a bug in Hasql or the server misbehaving. ",
            "Error: ",
            toPlainText (show resultError)
          ]
      Just (totalStatements, statementIndex, sql, parameters, prepared) ->
        StatementSessionError
          totalStatements
          statementIndex
          sql
          parameters
          prepared
          (fromStatementResultError resultError)
  Hipq.Recv.NoResultsError location details ->
    case location of
      Nothing ->
        (DriverSessionError . TextBuilder.toText . mconcat)
          [ "Unexpectedly got no results outside of statement context. ",
            "This indicates a bug in Hasql or the server misbehaving. ",
            "Details: ",
            toPlainText (show details)
          ]
      Just (totalStatements, statementIndex, sql, parameters, prepared) ->
        StatementSessionError
          totalStatements
          statementIndex
          sql
          parameters
          prepared
          (RowCountStatementError 1 1 0)
  Hipq.Recv.TooManyResultsError location actual ->
    case location of
      Nothing ->
        (DriverSessionError . TextBuilder.toText . mconcat)
          [ "Unexpectedly got too many results outside of statement context. ",
            "This indicates a bug in Hasql or the server misbehaving. ",
            "Amount: ",
            toPlainText actual
          ]
      Just (totalStatements, statementIndex, sql, parameters, prepared) ->
        StatementSessionError
          totalStatements
          statementIndex
          sql
          parameters
          prepared
          (RowCountStatementError 1 1 actual)

fromStatementResultError :: Hipq.ResultDecoder.Error -> StatementError
fromStatementResultError = \case
  Hipq.ResultDecoder.ServerError code message detail hint position ->
    ExecutionStatementError
      ( ExecutionError
          (decodeUtf8Lenient code)
          (decodeUtf8Lenient message)
          (fmap decodeUtf8Lenient detail)
          (fmap decodeUtf8Lenient hint)
          position
      )
  Hipq.ResultDecoder.UnexpectedResult msg ->
    UnexpectedResultStatementError msg
  Hipq.ResultDecoder.UnexpectedAmountOfRows actual ->
    RowCountStatementError 1 1 actual
  Hipq.ResultDecoder.UnexpectedAmountOfColumns expected actual ->
    UnexpectedAmountOfColumnsStatementError expected actual
  Hipq.ResultDecoder.DecoderTypeMismatch colIdx expectedOid actualOid ->
    -- Inline row error: create CellStatementError for decoder mismatch
    CellStatementError
      0
      colIdx
      (CellDeserializationError actualOid ("Decoder type mismatch. Expected " <> fromString (show expectedOid)))
  Hipq.ResultDecoder.RowError rowIndex rowError ->
    -- Inline RowError conversion
    case rowError of
      Hipq.ResultRowDecoder.CellError column cellErr ->
        CellStatementError
          rowIndex
          column
          ( case cellErr of
              Hipq.ResultRowDecoder.DecodingCellError oid msg -> CellDeserializationError (Pq.oidToWord32 oid) msg
              Hipq.ResultRowDecoder.UnexpectedNullCellError _ -> UnexpectedNullCellError
          )

fromRecvErrorInScript :: ByteString -> Hipq.Recv.Error (Maybe ByteString) -> SessionError
fromRecvErrorInScript scriptSql = \case
  Hipq.Recv.ResultError _ _ resultError ->
    case resultError of
      Hipq.ResultDecoder.ServerError code message detail hint position ->
        ScriptSessionError
          scriptSql
          ( ExecutionError
              (decodeUtf8Lenient code)
              (decodeUtf8Lenient message)
              (fmap decodeUtf8Lenient detail)
              (fmap decodeUtf8Lenient hint)
              position
          )
      Hipq.ResultDecoder.UnexpectedResult msg ->
        (DriverSessionError . TextBuilder.toText . mconcat)
          [ "Unexpected result in script: ",
            toPlainText msg
          ]
      Hipq.ResultDecoder.UnexpectedAmountOfRows actual ->
        (DriverSessionError . TextBuilder.toText . mconcat)
          [ "Unexpected amount of rows in script: ",
            toPlainText actual
          ]
      Hipq.ResultDecoder.UnexpectedAmountOfColumns expected actual ->
        (DriverSessionError . TextBuilder.toText . mconcat)
          [ "Unexpected amount of columns in script: expected ",
            toPlainText expected,
            ", got ",
            toPlainText actual
          ]
      Hipq.ResultDecoder.DecoderTypeMismatch colIdx expectedOid actualOid ->
        (DriverSessionError . TextBuilder.toText . mconcat)
          [ "Decoder type mismatch in script: expected OID ",
            toPlainText (show expectedOid),
            " at column ",
            toPlainText colIdx,
            ", got ",
            toPlainText (show actualOid)
          ]
      Hipq.ResultDecoder.RowError rowIndex rowError ->
        (DriverSessionError . TextBuilder.toText . mconcat)
          [ "Row error in script at row ",
            toPlainText rowIndex,
            ": ",
            toPlainText (show rowError)
          ]
  Hipq.Recv.NoResultsError _ details ->
    (DriverSessionError . TextBuilder.toText . mconcat)
      [ "Unexpectedly got no results in script. ",
        "This indicates a bug in Hasql or the server misbehaving. ",
        "Details: ",
        toPlainText (show details)
      ]
  Hipq.Recv.TooManyResultsError _ actual ->
    (DriverSessionError . TextBuilder.toText . mconcat)
      [ "Unexpectedly got too many results in script. ",
        "This indicates a bug in Hasql or the server misbehaving. ",
        "Amount: ",
        toPlainText actual
      ]

-- * Instances

instance ToPlainText ExecutionError where
  toPlainText (ExecutionError code message detail hint position) =
    mconcat
      [ toPlainText code,
        " - ",
        toPlainText message,
        maybe "" (\d -> " Detail: " <> toPlainText d) detail,
        maybe "" (\h -> " Hint: " <> toPlainText h) hint,
        maybe "" (\p -> " Position: " <> toPlainText (show p)) position
      ]

instance ToPlainText CellError where
  toPlainText = \case
    UnexpectedNullCellError ->
      "Unexpected null value"
    CellDeserializationError oid message ->
      mconcat
        [ "Failed to deserialize cell: ",
          toPlainText message,
          " (OID: ",
          toPlainText oid,
          ")"
        ]

instance ToPlainText StatementError where
  toPlainText = \case
    ExecutionStatementError executionError ->
      mconcat
        [ "Server error: ",
          toPlainText executionError
        ]
    UnexpectedResultStatementError message ->
      mconcat
        [ "Unexpected result: ",
          toPlainText message
        ]
    RowCountStatementError min max actual ->
      mconcat
        [ "Unexpected number of rows: expected ",
          if min == max
            then toPlainText min
            else toPlainText min <> " to " <> toPlainText max,
          ", got ",
          toPlainText actual
        ]
    UnexpectedAmountOfColumnsStatementError expected actual ->
      mconcat
        [ "Unexpected number of columns: expected ",
          toPlainText expected,
          ", got ",
          toPlainText actual
        ]
    CellStatementError rowIdx colIdx cellErr ->
      mconcat
        [ "In row ",
          TextBuilder.decimal rowIdx,
          ", column ",
          TextBuilder.decimal colIdx,
          ": ",
          toPlainText cellErr
        ]

instance ToPlainText SessionError where
  toPlainText = \case
    StatementSessionError totalStatements statementIndex sql parameters prepared statementError ->
      mconcat
        [ "In ",
          if prepared then "prepared" else "unprepared",
          " statement at offset ",
          TextBuilder.decimal statementIndex,
          " of pipeline with ",
          TextBuilder.decimal totalStatements,
          " statements.\n  SQL:\n    ",
          TextBuilderExtras.textWithEachLinePrefixed "    " (decodeUtf8Lenient sql),
          "\n  Params:\n    ",
          parameters
            & TextBuilder.intercalateMap "\n" (mappend "- " . TextBuilderExtras.textWithEachLinePrefixed "  ")
            & TextBuilderExtras.prefixEachLine "    ",
          "\n  Error: ",
          toPlainText statementError
        ]
    ScriptSessionError sql execErr ->
      mconcat
        [ "In script.\n  SQL:\n    ",
          TextBuilderExtras.textWithEachLinePrefixed "    " (decodeUtf8Lenient sql),
          "\n  Error: ",
          toPlainText execErr
        ]
    ConnectionSessionError message ->
      mconcat
        [ "Connection error: ",
          toPlainText message
        ]
    DriverSessionError message ->
      mconcat
        [ "Driver error: ",
          toPlainText message
        ]
