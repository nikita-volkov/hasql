module HighSQL.QQ.Parser where

import HighSQL.Prelude hiding (takeWhile)
import Data.Attoparsec.Text hiding (Result)
import qualified Data.Text as Text


-- |
-- The kind of a statement and the amount of placeholders.
type Result =
  (Kind, Int)

data Kind =
  Select |
  Update |
  Create
  deriving (Show, Read, Eq, Ord, Enum)

parse :: Text -> Either String Result
parse = 
  parseOnly statement
  where
    statement =
      (,) <$> (skipSpace *> kind) <*> countPlaceholders
    kind =
      asum (map assocToParser assocs)
      where
        assocToParser (word, kind) =
          asciiCI word *> pure kind
        assocs =
          do
            (kind, words) <- groups
            word <- words
            return (word, kind)
        groups =
          [
            (Select, ["select"]),
            (Update, ["update", "insert", "delete"]),
            (Create, ["create", "alter", "drop", "truncate"])
          ]
    countPlaceholders =
      count <|> pure 0
      where
        count =
          do
            many $ void stringLit <|> void (notChar '?')
            char '?'
            fmap succ countPlaceholders

stringLit :: Parser Text
stringLit =
  do
    quote <- 
      char '"' <|> char '\''
    text <- 
      fmap mconcat $ many $ 
        string "\\\\" <|> 
        string (fromString ['\\', quote]) <|> 
        (Text.singleton <$> notChar '"')
    char quote
    return text

