module Hasql.QQ.Parser where

import Hasql.Prelude hiding (takeWhile)
import Data.Attoparsec.Text hiding (Result)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB


type Result =
  (Text, [Param])

data Param =
  ParamName Text |
  OrderedPlaceholder |
  IndexedPlaceholder Int

-- |
-- Produces a whitespace-cleaned text and a count of placeholders in it.
parse :: Text -> Either String (Text, [Param])
parse = 
  parseOnly $ singleTemplate

singleTemplate :: Parser (Text, [Param])
singleTemplate =
  template <* 
  ((endOfInput) <|>
   (() <$ skipSpace <* char ';' <* fail "A semicolon detected, but only single statements are allowed"))

template :: Parser (Text, [Param])
template =
  runWriterT $ do
    lift $ skipSpace
    fmap (TL.toStrict . TLB.toLazyText . mconcat) $ 
      many $ 
        (mempty <$ lift (takeWhile1 isSpace <* endOfInput)) <|>
        (TLB.singleton ' ' <$ lift (takeWhile1 isSpace)) <|>
        (TLB.fromText <$> lift stringLit) <|>
        (TLB.singleton '?' <$ (lift param >>= tell . pure)) <|>
        (TLB.singleton <$> lift (notChar ';'))

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

param :: Parser Param
param =
  (char '$' *> ((ParamName <$> paramName) <|> (IndexedPlaceholder <$> decimal))) <|>
  (OrderedPlaceholder <$ char '?')

paramName :: Parser Text
paramName =
  T.cons <$> satisfy isLower <*> takeWhile (\c -> isAlphaNum c || elem c extraChars)
  where
    extraChars = "_'" :: [Char]
