module Core.Location where

import Platform.Prelude
import TextBuilder qualified

-- |
-- Location of an error in a pipeline of statements.
data InPipeline
  = InPipeline
      -- | Total number of statements in the pipeline.
      Int
  deriving stock (Show, Eq)

-- |
-- Location of an error in a statement.
data InStatement
  = InStatement
      -- | Location of the pipeline.
      InPipeline
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

instance ToPlainText InPipeline where
  toPlainText (InPipeline totalStatements) =
    mconcat
      [ "In pipeline with ",
        TextBuilder.decimal totalStatements,
        " statements"
      ]

instance ToPlainText InStatement where
  toPlainText (InStatement pipeline index sql params prepared) =
    mconcat
      [ "In statement ",
        TextBuilder.decimal index,
        " of ",
        toPlainText pipeline,
        ", SQL: ",
        TextBuilder.text (decodeUtf8Lenient sql),
        ", params: ",
        TextBuilder.intercalate (", ") (map TextBuilder.text params),
        ", prepared: ",
        TextBuilder.text (if prepared then "true" else "false")
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
