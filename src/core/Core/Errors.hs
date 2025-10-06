module Core.Errors where

import Hipq.Recv qualified
import Hipq.ResultDecoder qualified
import Hipq.ResultRowDecoder qualified
import Platform.Prelude
import Pq qualified
import TextBuilder qualified
import TextBuilderExtras qualified

-- * Location types

-- |
-- Location of an error in a statement.
data InStatement
  = InStatement
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
  deriving stock (Show, Eq)

-- |
-- Location of an error in a result row.
data InResultRow
  = InResultRow
      -- | Location of the statement.
      InStatement
      -- | 0-based index of the row.
      Int
  deriving stock (Show, Eq)

-- |
-- Location of an error in a result cell.
data InResultCell
  = InResultCell
      -- | Location of the row.
      InResultRow
      -- | 0-based index of the column.
      Int
  deriving stock (Show, Eq)

-- |
-- Location of an error in a script.
data InScript
  = InScript ByteString
  deriving stock (Show, Eq)

-- |
-- Location of an error in either a statement or a script.
data InStatementOrScript
  = ScriptInStatementOrScript InScript
  | StatementInStatementOrScript InStatement
  deriving stock (Show, Eq)

-- * Error types

-- |
-- Connection acquisition error.
data AcquisitionError
  = NetworkingAcquisitionError
      -- | Human readable details indended for logging.
      Text
  | AuthenticationAcquisitionError
      -- | Human readable details indended for logging.
      Text
  | CompatibilityAcquisitionError
      -- | Human readable details indended for logging.
      Text
  | -- | Uncategorized error coming from "libpq". May be empty text.
    OtherAcquisitionError
      -- | Human readable details intended for logging.
      Text
  deriving stock (Show, Eq)

data SessionError
  = UnexpectedNullCellSessionError
      -- | Location of the cell.
      InResultCell
  | CellDeserializationSessionError
      -- | Location of the cell.
      InResultCell
      -- | Type OID.
      Word32
      -- | Underlying error.
      Text
  | ServerSessionError
      -- | Location.
      InStatementOrScript
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
  | UnexpectedResultSessionError
      -- | Location of the statement.
      InStatement
      -- | Details.
      Text
  | RowCountSessionError
      -- | Location of the statement.
      InStatement
      -- | Expected minimum.
      Int
      -- | Expected maximum.
      Int
      -- | Actual.
      Int
  | UnexpectedAmountOfColumnsSessionError
      -- | Location of the statement.
      InStatement
      -- | Expected count.
      Int
      -- | Actual count.
      Int
  | ConnectionSessionError
      Text
  | -- | Either the server misbehaves or there is a bug in Hasql.
    DriverSessionError
      Text
  deriving stock (Show, Eq)

fromRecvError :: Hipq.Recv.Error (Maybe InStatement) -> SessionError
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
      Just stmtLocation ->
        fromStatementResultError stmtLocation resultError
  Hipq.Recv.NoResultsError location details ->
    case location of
      Nothing ->
        (DriverSessionError . TextBuilder.toText . mconcat)
          [ "Unexpectedly got no results outside of statement context. ",
            "This indicates a bug in Hasql or the server misbehaving. ",
            "Details: ",
            toPlainText (show details)
          ]
      Just stmtLocation ->
        RowCountSessionError stmtLocation 1 1 0
  Hipq.Recv.TooManyResultsError location actual ->
    case location of
      Nothing ->
        (DriverSessionError . TextBuilder.toText . mconcat)
          [ "Unexpectedly got too many results outside of statement context. ",
            "This indicates a bug in Hasql or the server misbehaving. ",
            "Amount: ",
            toPlainText actual
          ]
      Just stmtLocation ->
        RowCountSessionError stmtLocation 1 1 actual

fromStatementResultError :: InStatement -> Hipq.ResultDecoder.Error -> SessionError
fromStatementResultError stmtLocation = \case
  Hipq.ResultDecoder.ServerError code message detail hint position ->
    ServerSessionError
      (StatementInStatementOrScript stmtLocation)
      (decodeUtf8Lenient code)
      (decodeUtf8Lenient message)
      (fmap decodeUtf8Lenient detail)
      (fmap decodeUtf8Lenient hint)
      position
  Hipq.ResultDecoder.UnexpectedResult msg ->
    UnexpectedResultSessionError stmtLocation msg
  Hipq.ResultDecoder.UnexpectedAmountOfRows actual ->
    RowCountSessionError stmtLocation 1 1 actual
  Hipq.ResultDecoder.UnexpectedAmountOfColumns expected actual ->
    UnexpectedAmountOfColumnsSessionError stmtLocation expected actual
  Hipq.ResultDecoder.DecoderTypeMismatch colIdx expectedOid actualOid ->
    let rowLocation = InResultRow stmtLocation 0
        cellLocation = InResultCell rowLocation colIdx
     in CellDeserializationSessionError cellLocation actualOid ("Decoder type mismatch. Expected " <> fromString (show expectedOid))
  Hipq.ResultDecoder.RowError rowIndex rowError ->
    let rowLocation =
          InResultRow
            stmtLocation
            rowIndex
     in fromResultRowError rowLocation rowError

fromResultRowError :: InResultRow -> Hipq.ResultRowDecoder.Error -> SessionError
fromResultRowError rowLocation = \case
  Hipq.ResultRowDecoder.CellError column cellError ->
    let location =
          InResultCell
            rowLocation
            column
     in case cellError of
          Hipq.ResultRowDecoder.DecodingCellError oid message ->
            CellDeserializationSessionError location (Pq.oidToWord32 oid) message
          Hipq.ResultRowDecoder.UnexpectedNullCellError _oid ->
            UnexpectedNullCellSessionError location

fromRecvErrorInScript :: InScript -> Hipq.Recv.Error (Maybe InScript) -> SessionError
fromRecvErrorInScript scriptLocation = \case
  Hipq.Recv.ResultError _ _ resultError ->
    fromResultErrorInScript scriptLocation resultError
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

fromResultErrorInScript :: InScript -> Hipq.ResultDecoder.Error -> SessionError
fromResultErrorInScript scriptLocation = \case
  Hipq.ResultDecoder.ServerError code message detail hint position ->
    ServerSessionError
      (ScriptInStatementOrScript scriptLocation)
      (decodeUtf8Lenient code)
      (decodeUtf8Lenient message)
      (fmap decodeUtf8Lenient detail)
      (fmap decodeUtf8Lenient hint)
      position
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

-- * Instances

instance ToPlainText InStatement where
  toPlainText (InStatement total index sql params prepared) =
    mconcat
      [ "In ",
        if prepared then "prepared" else "unprepared",
        " statement at offset ",
        TextBuilder.decimal index,
        " of pipeline with ",
        TextBuilder.decimal total,
        " statements.\n  SQL:\n    ",
        TextBuilderExtras.textWithEachLinePrefixed "    " (decodeUtf8Lenient sql),
        "\n  Params:\n    ",
        params
          & TextBuilder.intercalateMap "\n" (mappend "- " . TextBuilderExtras.textWithEachLinePrefixed "  ")
          & TextBuilderExtras.prefixEachLine "    "
      ]

instance ToPlainText InResultRow where
  toPlainText (InResultRow statement rowIndex) =
    mconcat
      [ "In row ",
        TextBuilder.decimal rowIndex,
        " of ",
        toPlainText statement
      ]

instance ToPlainText InResultCell where
  toPlainText (InResultCell row columnIndex) =
    mconcat
      [ "In column ",
        TextBuilder.decimal columnIndex,
        " of ",
        toPlainText row
      ]

instance ToPlainText InScript where
  toPlainText (InScript sql) =
    mconcat
      [ "In script.\n  SQL:\n    ",
        TextBuilderExtras.textWithEachLinePrefixed "    " (decodeUtf8Lenient sql)
      ]

instance ToPlainText InStatementOrScript where
  toPlainText = \case
    ScriptInStatementOrScript scriptLocation -> toPlainText scriptLocation
    StatementInStatementOrScript statementLocation -> toPlainText statementLocation

instance ToPlainText SessionError where
  toPlainText = \case
    UnexpectedNullCellSessionError location ->
      mconcat
        [ "Unexpected null value in ",
          toPlainText location
        ]
    CellDeserializationSessionError location oid message ->
      mconcat
        [ "Failed to deserialize cell in ",
          toPlainText location,
          ": ",
          toPlainText message,
          " (OID: ",
          toPlainText oid,
          ")"
        ]
    ServerSessionError location code message detail hint position ->
      mconcat
        [ "Server error in ",
          toPlainText location,
          ": ",
          toPlainText code,
          " - ",
          toPlainText message,
          maybe "" (\d -> " Detail: " <> toPlainText d) detail,
          maybe "" (\h -> " Hint: " <> toPlainText h) hint,
          maybe "" (\p -> " Position: " <> toPlainText (show p)) position
        ]
    UnexpectedResultSessionError location message ->
      mconcat
        [ "Unexpected result in ",
          toPlainText location,
          ": ",
          toPlainText message
        ]
    RowCountSessionError location min max actual ->
      mconcat
        [ "Unexpected number of rows in ",
          toPlainText location,
          ": expected ",
          if min == max
            then toPlainText min
            else toPlainText min <> " to " <> toPlainText max,
          ", got ",
          toPlainText actual
        ]
    UnexpectedAmountOfColumnsSessionError location expected actual ->
      mconcat
        [ "Unexpected number of columns in ",
          toPlainText location,
          ": expected ",
          toPlainText expected,
          ", got ",
          toPlainText actual
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
