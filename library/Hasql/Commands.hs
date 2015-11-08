module Hasql.Commands
(
  Commands,
  asBytes,
  setEncodingToUTF8,
  setMinClientMessagesToWarning,
)
where

import Hasql.Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Builder as BB
import qualified Data.ByteString.Lazy.Builder.ASCII as BB
import qualified Data.ByteString.Lazy as BL


newtype Commands =
  Commands (DList BB.Builder)
  deriving (Monoid)

asBytes :: Commands -> ByteString
asBytes (Commands list) =
  BL.toStrict $ BB.toLazyByteString $ foldMap (<> BB.char7 ';') $ list

setEncodingToUTF8 :: Commands
setEncodingToUTF8 =
  Commands (pure "SET client_encoding = 'UTF8'")

setMinClientMessagesToWarning :: Commands
setMinClientMessagesToWarning =
  Commands (pure "SET client_min_messages TO WARNING")


