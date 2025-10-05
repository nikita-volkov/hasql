module Core.Location where

import Platform.Prelude
import TextBuilder qualified
import TextBuilderExtras qualified

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
