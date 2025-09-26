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
      $ [ TextBuilder.text "In pipeline with ",
          TextBuilder.decimal totalStatements,
          TextBuilder.text " statements"
        ]

instance ToPlainText InStatement where
  toPlainText (InStatement pipeline index sql params prepared) =
    mconcat
      $ [ TextBuilder.text "In statement ",
          TextBuilder.decimal index,
          TextBuilder.text " of ",
          toPlainText pipeline,
          TextBuilder.text ", SQL: ",
          TextBuilder.text (decodeUtf8Lenient sql),
          TextBuilder.text ", params: ",
          TextBuilder.intercalate (TextBuilder.text ", ") (map TextBuilder.text params),
          TextBuilder.text ", prepared: ",
          TextBuilder.text (if prepared then "true" else "false")
        ]

instance ToPlainText InResultRow where
  toPlainText (InResultRow statement rowIndex) =
    mconcat
      $ [ TextBuilder.text "In row ",
          TextBuilder.decimal rowIndex,
          TextBuilder.text " of ",
          toPlainText statement
        ]

instance ToPlainText InResultCell where
  toPlainText (InResultCell row columnIndex) =
    mconcat
      $ [ TextBuilder.text "In column ",
          TextBuilder.decimal columnIndex,
          TextBuilder.text " of ",
          toPlainText row
        ]
