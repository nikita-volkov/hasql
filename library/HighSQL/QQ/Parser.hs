module HighSQL.QQ.Parser where

import HighSQL.Prelude hiding (takeWhile)
import Data.Attoparsec.Text hiding (Result)
import qualified Data.Text as Text
import qualified Language.Haskell.TH as TH


-- |
-- The kind of a statement and the amount of placeholders.
type Result =
  (Statement, Int)

data Statement =
  Select | Update | Insert | Delete | Create | Alter | Drop | Truncate
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
          [("select", Select), ("update", Update), ("insert", Insert),
           ("delete", Delete), ("create", Create), ("alter", Alter),
           ("drop", Drop), ("truncate", Truncate)]
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

