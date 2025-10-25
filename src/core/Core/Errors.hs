module Core.Errors where

import Hipq.Recv qualified
import Hipq.ResultDecoder qualified
import Hipq.Roundtrip qualified
import Hipq.RowReader qualified
import Platform.Prelude
import TextBuilder qualified

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
      -- | SQL State Code.
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
  | DeserializationCellError
      -- | Underlying error.
      Text
  deriving stock (Show, Eq)

-- |
-- Statement-level error.
data StatementError
  = -- | The server rejected the statement with an error.
    ExecutionStatementError ExecutionError
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
  | UnexpectedColumnTypeStatementError
      -- | 0-based column index.
      Int
      -- | Expected type OID.
      Word32
      -- | Actual type OID.
      Word32
  | CellStatementError
      -- | 0-based row index.
      Int
      -- | 0-based column index.
      Int
      -- | Underlying cell error.
      CellError
  | -- | The database returned an unexpected result.
    -- Indicates an improper statement or a schema mismatch.
    UnexpectedResultStatementError
      -- | Details.
      Text
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
  | -- | One or more types referenced in the statement could not be found in the database.
    MissingTypesSessionError
      -- | Set of (schema name, type name) pairs that could not be found.
      (HashSet (Maybe Text, Text))
  deriving stock (Show, Eq)

fromRoundtripError :: Hipq.Roundtrip.Error context -> SessionError
fromRoundtripError = \case
  Hipq.Roundtrip.ClientError _context details ->
    ConnectionSessionError (maybe "" decodeUtf8Lenient details)
  Hipq.Roundtrip.ServerError recvError ->
    fromRecvError (Nothing <$ recvError)

fromRecvError :: Hipq.Recv.Error (Maybe (Int, Int, ByteString, [Text], Bool)) -> SessionError
fromRecvError = \case
  Hipq.Recv.ResultError location _resultOffset resultError ->
    case location of
      Nothing ->
        (DriverSessionError . TextBuilder.toText . mconcat)
          [ "Unexpected error outside of statement context. ",
            "This indicates a bug in Hasql or the server misbehaving. ",
            "Error: ",
            TextBuilder.string (show resultError)
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
            TextBuilder.string (show details)
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
            TextBuilder.decimal actual
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
    UnexpectedColumnTypeStatementError
      colIdx
      expectedOid
      actualOid
  Hipq.ResultDecoder.RowError rowIndex rowError ->
    case rowError of
      Hipq.RowReader.CellError column _oid cellErr ->
        CellStatementError
          rowIndex
          column
          ( case cellErr of
              Hipq.RowReader.DecodingCellError msg -> DeserializationCellError msg
              Hipq.RowReader.UnexpectedNullCellError -> UnexpectedNullCellError
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
            TextBuilder.text msg
          ]
      Hipq.ResultDecoder.UnexpectedAmountOfRows actual ->
        (DriverSessionError . TextBuilder.toText . mconcat)
          [ "Unexpected amount of rows in script: ",
            TextBuilder.decimal actual
          ]
      Hipq.ResultDecoder.UnexpectedAmountOfColumns expected actual ->
        (DriverSessionError . TextBuilder.toText . mconcat)
          [ "Unexpected amount of columns in script: expected ",
            TextBuilder.decimal expected,
            ", got ",
            TextBuilder.decimal actual
          ]
      Hipq.ResultDecoder.DecoderTypeMismatch colIdx expectedOid actualOid ->
        (DriverSessionError . TextBuilder.toText . mconcat)
          [ "Decoder type mismatch in script: expected OID ",
            TextBuilder.string (show expectedOid),
            " at column ",
            TextBuilder.decimal colIdx,
            ", got ",
            TextBuilder.string (show actualOid)
          ]
      Hipq.ResultDecoder.RowError rowIndex rowError ->
        (DriverSessionError . TextBuilder.toText . mconcat)
          [ "Row error in script at row ",
            TextBuilder.decimal rowIndex,
            ": ",
            TextBuilder.string (show rowError)
          ]
  Hipq.Recv.NoResultsError _ details ->
    (DriverSessionError . TextBuilder.toText . mconcat)
      [ "Got no results in script.",
        " This indicates a bug in Hasql or the server misbehaving.",
        details
          & filter (/= "")
          & foldMap (mappend " Details: " . TextBuilder.text . decodeUtf8Lenient)
      ]
  Hipq.Recv.TooManyResultsError _ actual ->
    (DriverSessionError . TextBuilder.toText . mconcat)
      [ "Got too many results in script. ",
        "This indicates a bug in Hasql or the server misbehaving. ",
        "Amount: ",
        TextBuilder.decimal actual
      ]
