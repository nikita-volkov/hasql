module PercentEncoding.Parsers where

import Control.Exception qualified as Exception
import Data.ByteString qualified as ByteString
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error qualified as Text.Encoding
import PercentEncoding.MonadPlus qualified as MonadPlus
import Platform.Prelude hiding (try)
import Text.Megaparsec
import Text.Megaparsec.Char
import TextBuilder qualified as TextBuilder

type Parser = Parsec Void Text

{-# INLINEABLE urlEncodedComponentText #-}
urlEncodedComponentText :: (Char -> Bool) -> Parser Text
urlEncodedComponentText stopCharPredicate =
  labeled "URL-encoded component"
    $ fmap TextBuilder.toText
    $ MonadPlus.scanl1 mappend
    $ parser
  where
    parser =
      asum
        [ TextBuilder.text <$> try (takeWhile1P (Just "Unencoded char") unencodedCharPredicate),
          urlEncodedSequenceTextBuilder
        ]
      where
        unencodedCharPredicate c =
          not (c == '%' || stopCharPredicate c)

{-# INLINEABLE urlEncodedSequenceTextBuilder #-}
urlEncodedSequenceTextBuilder :: Parser TextBuilder.TextBuilder
urlEncodedSequenceTextBuilder =
  labeled "URL-encoded sequence" $ do
    start <- progress (mempty, mempty, Text.Encoding.streamDecodeUtf8) =<< urlEncodedByte
    MonadPlus.scanlM progress start urlEncodedByte >>= finish
  where
    progress (!builder, _ :: ByteString, decode) byte =
      case unsafeDupablePerformIO (Exception.try (evaluate (decode (ByteString.singleton byte)))) of
        Right (Text.Encoding.Some decodedChunk undecodedBytes newDecode) ->
          return (builder <> TextBuilder.text decodedChunk, undecodedBytes, newDecode)
        Left (Text.Encoding.DecodeError error _) ->
          fail (showString "UTF8 decoding: " error)
        Left _ ->
          fail "Unexpected decoding error"
    finish (builder, undecodedBytes, _) =
      if ByteString.null undecodedBytes
        then return builder
        else fail (showString "UTF8 decoding: Bytes remaining: " (show undecodedBytes))

{-# INLINE urlEncodedByte #-}
urlEncodedByte :: Parser Word8
urlEncodedByte = do
  _ <- try (char '%')
  digit1 <- fromIntegral <$> hexadecimalDigit
  digit2 <- fromIntegral <$> hexadecimalDigit
  return (shiftL digit1 4 .|. digit2)

{-# INLINE hexadecimalDigit #-}
hexadecimalDigit :: Parser Int
hexadecimalDigit = label "Hex digit" do
  c <- anySingle
  let x = ord c
  if x >= 48 && x < 58
    then return (x - 48)
    else
      if x >= 65 && x < 71
        then return (x - 55)
        else
          if x >= 97 && x < 103
            then return (x - 97)
            else fail ("Not a hexadecimal digit: " <> show c)

{-# INLINE labeled #-}
labeled :: String -> Parser a -> Parser a
labeled label parser =
  parser <?> label
