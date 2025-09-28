module TextBuilderExtras where

import Data.Text qualified as Text
import Platform.Prelude
import TextBuilder

textWithEachLinePrefixed :: TextBuilder -> Text -> TextBuilder
textWithEachLinePrefixed prefix =
  intercalateMap "\n" (mappend prefix . text) . Text.lines

prefixEachLine :: TextBuilder -> TextBuilder -> TextBuilder
prefixEachLine prefix =
  textWithEachLinePrefixed prefix . toText
