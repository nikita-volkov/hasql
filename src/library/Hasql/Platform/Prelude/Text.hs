{-# LANGUAGE PackageImports #-}

module Hasql.Platform.Prelude.Text where

import Data.ByteString qualified
import Data.Text qualified
import Data.Text.Encoding qualified
import Data.Text.Encoding.Error qualified
import Data.Text.Lazy qualified

type LazyText =
  Data.Text.Lazy.Text

decodeUtf8Lenient :: Data.ByteString.ByteString -> Data.Text.Text
decodeUtf8Lenient =
  Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode
