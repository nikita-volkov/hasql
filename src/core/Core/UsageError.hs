module Core.UsageError where

import Core.Location
import Hipq.Recv qualified
import Hipq.ResultDecoder qualified
import Hipq.ResultRowDecoder qualified
import Platform.Prelude
import Pq qualified
import TextBuilder qualified

data UsageError
  = UnexpectedNullCellUsageError
      -- | Location of the cell.
      InResultCell
  | CellDeserializationUsageError
      -- | Location of the cell.
      InResultCell
      -- | Type OID.
      Word32
      -- | Underlying error.
      Text
  | ServerUsageError
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
  | UnexpectedResultUsageError
      -- | Location of the statement.
      InStatement
      -- | Details.
      Text
  | RowCountUsageError
      -- | Location of the statement.
      InStatement
      -- | Expected minimum.
      Int
      -- | Expected maximum.
      Int
      -- | Actual.
      Int
  | UnexpectedAmountOfColumnsUsageError
      -- | Location of the statement.
      InStatement
      -- | Expected count.
      Int
      -- | Actual count.
      Int
  | ConnectionUsageError
      Text
  | -- | Either the server misbehaves or there is a bug in Hasql.
    DriverUsageError
      Text
  deriving stock (Show, Eq)

fromRecvError :: Hipq.Recv.Error (Maybe InStatement) -> UsageError
fromRecvError = \case
  Hipq.Recv.ResultError location _resultOffset resultError ->
    case location of
      Nothing ->
        (DriverUsageError . TextBuilder.toText . mconcat)
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
        (DriverUsageError . TextBuilder.toText . mconcat)
          [ "Unexpectedly got no results outside of statement context. ",
            "This indicates a bug in Hasql or the server misbehaving. ",
            "Details: ",
            toPlainText (show details)
          ]
      Just stmtLocation ->
        RowCountUsageError stmtLocation 1 1 0
  Hipq.Recv.TooManyResultsError location actual ->
    case location of
      Nothing ->
        (DriverUsageError . TextBuilder.toText . mconcat)
          [ "Unexpectedly got too many results outside of statement context. ",
            "This indicates a bug in Hasql or the server misbehaving. ",
            "Amount: ",
            toPlainText actual
          ]
      Just stmtLocation ->
        RowCountUsageError stmtLocation 1 1 actual

fromStatementResultError :: InStatement -> Hipq.ResultDecoder.Error -> UsageError
fromStatementResultError stmtLocation = \case
  Hipq.ResultDecoder.ServerError code message detail hint position ->
    ServerUsageError
      stmtLocation
      (decodeUtf8Lenient code)
      (decodeUtf8Lenient message)
      (fmap decodeUtf8Lenient detail)
      (fmap decodeUtf8Lenient hint)
      position
  Hipq.ResultDecoder.UnexpectedResult msg ->
    UnexpectedResultUsageError stmtLocation msg
  Hipq.ResultDecoder.UnexpectedAmountOfRows actual ->
    RowCountUsageError stmtLocation 1 1 actual
  Hipq.ResultDecoder.UnexpectedAmountOfColumns expected actual ->
    UnexpectedAmountOfColumnsUsageError stmtLocation expected actual
  Hipq.ResultDecoder.DecoderTypeMismatch colIdx expectedOid actualOid ->
    let rowLocation = InResultRow stmtLocation 0
        cellLocation = InResultCell rowLocation colIdx
     in CellDeserializationUsageError cellLocation actualOid ("Decoder type mismatch. Expected " <> fromString (show expectedOid))
  Hipq.ResultDecoder.RowError rowIndex rowError ->
    let rowLocation =
          InResultRow
            stmtLocation
            rowIndex
     in fromResultRowError rowLocation rowError

fromResultRowError :: InResultRow -> Hipq.ResultRowDecoder.Error -> UsageError
fromResultRowError rowLocation = \case
  Hipq.ResultRowDecoder.CellError column cellError ->
    let location =
          InResultCell
            rowLocation
            column
     in case cellError of
          Hipq.ResultRowDecoder.DecodingCellError oid message ->
            CellDeserializationUsageError location (Pq.oidToWord32 oid) message
          Hipq.ResultRowDecoder.UnexpectedNullCellError _oid ->
            UnexpectedNullCellUsageError location

instance ToPlainText UsageError where
  toPlainText = \case
    UnexpectedNullCellUsageError location ->
      mconcat
        [ "Unexpected null value in ",
          toPlainText location
        ]
    CellDeserializationUsageError location oid message ->
      mconcat
        [ "Failed to deserialize cell in ",
          toPlainText location,
          ": ",
          toPlainText message,
          " (OID: ",
          toPlainText oid,
          ")"
        ]
    ServerUsageError location code message detail hint position ->
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
    UnexpectedResultUsageError location message ->
      mconcat
        [ "Unexpected result in ",
          toPlainText location,
          ": ",
          toPlainText message
        ]
    RowCountUsageError location min max actual ->
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
    UnexpectedAmountOfColumnsUsageError location expected actual ->
      mconcat
        [ "Unexpected number of columns in ",
          toPlainText location,
          ": expected ",
          toPlainText expected,
          ", got ",
          toPlainText actual
        ]
    ConnectionUsageError message ->
      mconcat
        [ "Connection error: ",
          toPlainText message
        ]
    DriverUsageError message ->
      mconcat
        [ "Driver error: ",
          toPlainText message
        ]
