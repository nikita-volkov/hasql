module Core.Errors where

import Core.Location
import Hipq.Recv qualified
import Hipq.ResultDecoder qualified
import Hipq.ResultRowDecoder qualified
import Platform.Prelude
import Pq qualified
import TextBuilder qualified

data Error
  = UnexpectedNullCellError
      -- | Location of the cell.
      InResultCell
  | CellDeserializationError
      -- | Location of the cell.
      InResultCell
      -- | Type OID.
      Word32
      -- | Underlying error.
      Text
  | ServerError
      -- | Location of the statement.
      InStatement
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
  | UnexpectedResultError
      -- | Location of the statement.
      InStatement
      -- | Details.
      Text
  | RowCountError
      -- | Location of the statement.
      InStatement
      -- | Expected minimum.
      Int
      -- | Expected maximum.
      Int
      -- | Actual.
      Int
  | UnexpectedAmountOfColumnsError
      -- | Location of the statement.
      InStatement
      -- | Expected count.
      Int
      -- | Actual count.
      Int
  | ConnectionError
      Text
  | -- | Either the server misbehaves or there is a bug in Hasql.
    DriverError
      Text
  deriving stock (Show, Eq)

fromRecvError :: Hipq.Recv.Error (Either InPipeline InStatement) -> Error
fromRecvError = \case
  Hipq.Recv.ResultError location _resultOffset resultError ->
    case location of
      Left pipelineLocation ->
        (DriverError . TextBuilder.toText . mconcat)
          [ "Unexpected error outside of statement context. ",
            "This indicates a bug in Hasql or the server misbehaving. ",
            "Context: ",
            toPlainText pipelineLocation,
            ". ",
            "Error: ",
            toPlainText (show resultError)
          ]
      Right stmtLocation ->
        fromStatementResultError stmtLocation resultError
  Hipq.Recv.NoResultsError location details ->
    case location of
      Left pipelineLocation ->
        (DriverError . TextBuilder.toText . mconcat)
          [ "Unexpectedly got no results outside of statement context. ",
            "This indicates a bug in Hasql or the server misbehaving. ",
            "Context: ",
            toPlainText pipelineLocation,
            ". ",
            "Details: ",
            toPlainText (show details)
          ]
      Right stmtLocation ->
        RowCountError stmtLocation 1 1 0
  Hipq.Recv.TooManyResultsError location actual ->
    case location of
      Left pipelineLocation ->
        (DriverError . TextBuilder.toText . mconcat)
          [ "Unexpectedly got too many results outside of statement context. ",
            "This indicates a bug in Hasql or the server misbehaving. ",
            "Context: ",
            toPlainText pipelineLocation,
            ". ",
            "Amount: ",
            toPlainText actual
          ]
      Right stmtLocation ->
        RowCountError stmtLocation 1 1 actual

fromStatementResultError :: InStatement -> Hipq.ResultDecoder.Error -> Error
fromStatementResultError stmtLocation = \case
  Hipq.ResultDecoder.ServerError code message detail hint position ->
    ServerError
      stmtLocation
      (decodeUtf8Lenient code)
      (decodeUtf8Lenient message)
      (fmap decodeUtf8Lenient detail)
      (fmap decodeUtf8Lenient hint)
      position
  Hipq.ResultDecoder.UnexpectedResult msg ->
    UnexpectedResultError stmtLocation msg
  Hipq.ResultDecoder.UnexpectedAmountOfRows actual ->
    RowCountError stmtLocation 1 1 actual
  Hipq.ResultDecoder.UnexpectedAmountOfColumns expected actual ->
    UnexpectedAmountOfColumnsError stmtLocation expected actual
  Hipq.ResultDecoder.DecoderTypeMismatch colIdx expectedOid actualOid ->
    let rowLocation = InResultRow stmtLocation 0
        cellLocation = InResultCell rowLocation colIdx
     in CellDeserializationError cellLocation actualOid ("Decoder type mismatch. Expected " <> fromString (show expectedOid))
  Hipq.ResultDecoder.RowError rowIndex rowError ->
    let rowLocation =
          InResultRow
            stmtLocation
            rowIndex
     in fromResultRowError rowLocation rowError

fromResultRowError :: InResultRow -> Hipq.ResultRowDecoder.Error -> Error
fromResultRowError rowLocation = \case
  Hipq.ResultRowDecoder.CellError column cellError ->
    let location =
          InResultCell
            rowLocation
            column
     in case cellError of
          Hipq.ResultRowDecoder.DecodingCellError oid message ->
            CellDeserializationError location (Pq.oidToWord32 oid) message
          Hipq.ResultRowDecoder.UnexpectedNullCellError _oid ->
            UnexpectedNullCellError location

instance ToPlainText Error where
  toPlainText = \case
    UnexpectedNullCellError location ->
      mconcat
        [ "Unexpected null value in ",
          toPlainText location
        ]
    CellDeserializationError location oid message ->
      mconcat
        [ "Failed to deserialize cell in ",
          toPlainText location,
          ": ",
          toPlainText message,
          " (OID: ",
          toPlainText oid,
          ")"
        ]
    ServerError location code message detail hint position ->
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
    UnexpectedResultError location message ->
      mconcat
        [ "Unexpected result in ",
          toPlainText location,
          ": ",
          toPlainText message
        ]
    RowCountError location min max actual ->
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
    UnexpectedAmountOfColumnsError location expected actual ->
      mconcat
        [ "Unexpected number of columns in ",
          toPlainText location,
          ": expected ",
          toPlainText expected,
          ", got ",
          toPlainText actual
        ]
    ConnectionError message ->
      mconcat
        [ "Connection error: ",
          toPlainText message
        ]
    DriverError message ->
      mconcat
        [ "Driver error: ",
          toPlainText message
        ]
