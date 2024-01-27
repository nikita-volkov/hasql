module Hasql.Statement.Function.SqlBuilder where

import ByteString.StrictBuilder
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Hasql.Prelude

sql :: Text -> Int -> Builder
sql name size =
  "select " <> functionName name <> "(" <> arguments size <> ")"

functionName :: Text -> Builder
functionName =
  mappend "\"" . flip mappend "\"" . bytes . Text.encodeUtf8 . Text.replace "\"" ""

arguments :: Int -> Builder
arguments size =
  [1 .. size]
    & fmap argument
    & intersperse ", "
    & mconcat

argument :: Int -> Builder
argument num =
  "$" <> asciiIntegral num
