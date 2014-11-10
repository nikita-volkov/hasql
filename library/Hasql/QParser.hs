module Hasql.QParser where

import Hasql.Prelude hiding (takeWhile)
import Data.Attoparsec.Text hiding (Result)
import qualified Data.Text as Text


-- |
-- The amount of placeholders.
type Result =
  (Word)

parse :: Text -> Either String Result
parse = 
  parseOnly countPlaceholders
  where
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
        (Text.singleton <$> notChar quote)
    char quote
    return text

