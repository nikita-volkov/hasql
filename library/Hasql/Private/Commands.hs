module Hasql.Private.Commands
(
  Commands,
  asBytes,
  setEncodersToUTF8,
  setMinClientMessagesToWarning,
)
where

import Hasql.Private.Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Builder as BB
import qualified Data.ByteString.Lazy.Builder.ASCII as BB
import qualified Data.ByteString.Lazy as BL


newtype Commands =
  Commands (DList BB.Builder)
  deriving (Semigroup, Monoid)

asBytes :: Commands -> ByteString
asBytes (Commands list) =
  BL.toStrict $ BB.toLazyByteString $ foldMap (<> BB.char7 ';') $ list

setEncodersToUTF8 :: Commands
setEncodersToUTF8 =
  Commands (pure "SET client_encoding = 'UTF8'")

setMinClientMessagesToWarning :: Commands
setMinClientMessagesToWarning =
  Commands (pure "SET client_min_messages TO WARNING")
