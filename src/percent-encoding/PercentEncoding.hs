module PercentEncoding where

import PercentEncoding.Parsers qualified as Parsers
import PercentEncoding.TextBuilders qualified as TextBuilders
import Platform.Prelude
import Text.Megaparsec qualified as Megaparsec
import TextBuilder (TextBuilder)

encodeText :: Text -> TextBuilder
encodeText = TextBuilders.urlEncodedText

parser ::
  -- | Test on stop-char. @%@ is already accounted for.
  (Char -> Bool) ->
  -- | Megaparsec parser for a percent-encoded text component.
  Megaparsec.Parsec Void Text Text
parser isStopChar =
  Parsers.urlEncodedComponentText isStopChar
