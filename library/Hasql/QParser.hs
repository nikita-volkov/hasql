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
  parseOnly $ flip execStateT 0 $
    statement *>
    (lift endOfInput <|> 
     (void (lift (char ';')) <* fail "A semicolon detected. Only single statements are allowed"))
  where
    statement =
      skipMany1 $ 
        void (lift stringLit) <|> 
        void (lift (char '?') <* modify succ) <|>
        void (lift (notChar ';'))

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

