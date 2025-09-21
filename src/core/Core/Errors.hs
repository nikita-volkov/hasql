module Core.Errors where

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
  | NotEnoughRowsError
      -- | Location of the statement.
      InStatement
      -- | Expected minimum count.
      Int
      -- | Actual count.
      Int
  | TooManyRowsError
      -- | Location of the statement.
      InStatement
      -- | Expected maximum count.
      Int
      -- | Actual count.
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

data InPipeline = InPipeline
  { totalStatements :: Int
  }
  deriving stock (Show, Eq)

data InStatement = InStatement
  { pipeline :: InPipeline,
    indexInPipeline :: Int,
    sql :: ByteString,
    params :: [Text],
    prepared :: Bool
  }
  deriving stock (Show, Eq)

data InResultRow = InResultRow
  { statement :: InStatement,
    -- | Row index.
    rowIndex :: Int
  }
  deriving stock (Show, Eq)

data InResultCell = InResultCell
  { resultRow :: InResultRow,
    -- | Column index.
    columnIndex :: Int
  }
  deriving stock (Show, Eq)

instance ToPlainText InPipeline where
  toPlainText (InPipeline totalStatements) =
    "In pipeline with " <> TextBuilder.decimal totalStatements <> " statements"

fromRecvError :: Hipq.Recv.Error (Either InPipeline InStatement) -> Error
fromRecvError = \case
  Hipq.Recv.ResultError location _resultOffset resultError ->
    case location of
      Left pipelineLocation ->
        DriverError message
        where
          message =
            (TextBuilder.toText . mconcat)
              [ "Unexpected error outside of statement context.\n",
                "This indicates a bug in Hasql or the server misbehaving.\n",
                "Context: ",
                toPlainText pipelineLocation,
                "\n",
                "Error: ",
                toPlainText (show resultError)
              ]
      Right stmtLocation ->
        fromStatementResultError stmtLocation resultError

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
    DriverError msg
  Hipq.ResultDecoder.UnexpectedAmountOfRows actual ->
    TooManyRowsError stmtLocation 1 actual
  Hipq.ResultDecoder.UnexpectedAmountOfColumns expected actual ->
    UnexpectedAmountOfColumnsError stmtLocation expected actual
  Hipq.ResultDecoder.DecoderTypeMismatch colIdx expectedOid actualOid ->
    let rowLocation = InResultRow {statement = stmtLocation, rowIndex = 0}
        cellLocation = InResultCell {resultRow = rowLocation, columnIndex = colIdx}
     in CellDeserializationError cellLocation actualOid ("Decoder type mismatch. Expected " <> fromString (show expectedOid))
  Hipq.ResultDecoder.RowError rowIndex rowError ->
    let rowLocation =
          InResultRow
            { statement = stmtLocation,
              rowIndex = rowIndex
            }
     in fromResultRowError rowLocation rowError

fromResultRowError :: InResultRow -> Hipq.ResultRowDecoder.Error -> Error
fromResultRowError rowLocation = \case
  Hipq.ResultRowDecoder.CellError column cellError ->
    let location =
          InResultCell
            { resultRow = rowLocation,
              columnIndex = column
            }
     in case cellError of
          Hipq.ResultRowDecoder.DecodingCellError oid message ->
            CellDeserializationError location (Pq.oidToWord32 oid) message
          Hipq.ResultRowDecoder.UnexpectedNullCellError _oid ->
            UnexpectedNullCellError location
