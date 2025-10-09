module PercentEncoding where

import PercentEncoding.TextBuilders qualified as TextBuilders
import Platform.Prelude
import TextBuilder (TextBuilder)

encodeText :: Text -> TextBuilder
encodeText = TextBuilders.urlEncodedText
