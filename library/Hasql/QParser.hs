module Hasql.QParser where

import Hasql.Prelude hiding (takeWhile)
import Data.Attoparsec.Text hiding (Result)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB


-- |
-- Produces a whitespace-cleaned text and a count of placeholders in it.
parse :: Text -> Either String (Text, Int)
parse = 
  parseOnly $ singleTemplate

singleTemplate :: Parser (Text, Int)
singleTemplate =
  template <* 
  ((endOfInput) <|>
   (() <$ skipSpace <* char ';' <* fail "A semicolon detected. Only single statements are allowed"))

template :: Parser (Text, Int)
template =
  flip runStateT 0 $ do
    lift $ skipSpace
    fmap (TL.toStrict . TLB.toLazyText . mconcat) $ 
      many $ 
        (mempty <$ lift trailingWhitespace) <|>
        (TLB.singleton ' ' <$ lift (takeWhile1 isSpace)) <|>
        (TLB.fromText <$> lift stringLit) <|>
        (TLB.singleton <$> lift (char '?') <* modify succ) <|>
        (TLB.singleton <$> lift (notChar ';'))

trailingWhitespace :: Parser ()
trailingWhitespace =
  () <$ takeWhile1 isSpace <* endOfInput

stringLit :: Parser Text
stringLit =
  do
    quote <- 
      char '"' <|> char '\''
    content <- 
      fmap mconcat $ many $ 
        TLB.fromText <$> string "\\\\" <|> 
        TLB.fromText <$> string (fromString ['\\', quote]) <|> 
        TLB.singleton <$> notChar quote
    char quote
    return $ TL.toStrict . TLB.toLazyText $
      TLB.singleton quote <> content <> TLB.singleton quote
