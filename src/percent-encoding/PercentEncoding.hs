module PercentEncoding where

import Data.Attoparsec.Text qualified as Attoparsec
import PercentEncoding.AttoparsecParsers qualified as AttoparsecParsers
import PercentEncoding.TextBuilders qualified as TextBuilders
import Platform.Prelude
import TextBuilder (TextBuilder)

encodeText :: Text -> TextBuilder
encodeText = TextBuilders.urlEncodedText

attoparsecParser ::
  -- | Test on stop-char. @%@ is already accounted for.
  (Char -> Bool) ->
  -- | Parser for a percent-encoded text component.
  Attoparsec.Parser Text
attoparsecParser isStopChar =
  AttoparsecParsers.urlEncodedComponentText isStopChar
