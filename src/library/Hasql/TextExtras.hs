module Hasql.TextExtras where

import Data.Text qualified as Text
import Hasql.Platform.Prelude

prefixEachLine :: Text -> Text -> Text
prefixEachLine prefix =
  Text.intercalate "\n" . fmap (mappend prefix) . Text.lines
