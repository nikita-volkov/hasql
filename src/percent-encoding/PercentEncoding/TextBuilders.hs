module PercentEncoding.TextBuilders where

import Data.CharSet qualified as CharSet
import Data.Text qualified as Text
import PercentEncoding.Charsets qualified as Charsets
import PercentEncoding.Utf8CharView qualified as Utf8CharView
import Platform.Prelude
import TextBuilder

-- | Apply URL-encoding to text
urlEncodedText :: Text -> TextBuilder
urlEncodedText =
  foldMap urlEncodedChar . Text.unpack

urlEncodedChar :: Char -> TextBuilder
urlEncodedChar c =
  if CharSet.member c Charsets.passthrough
    then char c
    else
      Utf8CharView.char
        c
        ( \b1 ->
            urlEncodedByte b1
        )
        ( \b1 b2 ->
            urlEncodedByte b1 <> urlEncodedByte b2
        )
        ( \b1 b2 b3 ->
            mconcat [urlEncodedByte b1, urlEncodedByte b2, urlEncodedByte b3]
        )
        ( \b1 b2 b3 b4 ->
            mconcat [urlEncodedByte b1, urlEncodedByte b2, urlEncodedByte b3, urlEncodedByte b4]
        )

urlEncodedByte :: Word8 -> TextBuilder
urlEncodedByte x = char '%' <> hexadecimal x
