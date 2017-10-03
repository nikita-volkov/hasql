module Hasql.Protocol.Decoding where

import Hasql.Prelude
import Hasql.Protocol.Model
import BinaryParser


{-# INLINE word8 #-}
word8 :: BinaryParser Word8
word8 =
  byte

{-# INLINE word16 #-}
word16 :: BinaryParser Word16
word16 =
  beWord16

{-# INLINE word32 #-}
word32 :: BinaryParser Word32
word32 =
  beWord32

{-# INLINE int32 #-}
int32 :: BinaryParser Int32
int32 =
  fromIntegral <$> beWord32

{-# INLINE messageTypeAndLength #-}
messageTypeAndLength :: (MessageType -> PayloadLength -> a) -> BinaryParser a
messageTypeAndLength cont =
  cont <$> messageType <*> payloadLength

{-# INLINE messageType #-}
messageType :: BinaryParser MessageType
messageType =
  MessageType <$> word8

{-# INLINE payloadLength #-}
payloadLength :: BinaryParser PayloadLength
payloadLength =
  PayloadLength . subtract 4 . fromIntegral <$> word32

{-# INLINE nullableSizedValue #-}
nullableSizedValue :: BinaryParser a -> BinaryParser (Maybe a)
nullableSizedValue value =
  do
    size <- int32
    case size of
      -1 -> return Nothing
      _ -> sized (fromIntegral size) (fmap Just value)

{-# INLINE sizedValue #-}
sizedValue :: BinaryParser a -> BinaryParser a
sizedValue value =
  do
    size <- int32
    case size of
      -1 -> failure "Unexpected null"
      _ -> sized (fromIntegral size) value

{-|
Extracts the number of affected rows from the body of the CommandComplete message.
-}
{-# INLINE commandCompleteMessageAffectedRows #-}
commandCompleteMessageAffectedRows :: BinaryParser Int
commandCompleteMessageAffectedRows =
  do
    header <- bytesWhile byteIsUpperLetter
    byte
    case header of
      "INSERT" -> unitWhile byteIsDecimal *> byte *> asciiIntegral <* byte
      _ -> asciiIntegral <* byte
  where
    byteIsUpperLetter byte =
      byte - 65 <= 25
    byteIsDecimal byte =
      byte - 48 <= 9

{-|
The essential components of the error (or notice) message.
-}
{-# INLINE errorMessage #-}
errorMessage :: (ByteString -> ByteString -> errorMessage) -> BinaryParser errorMessage
errorMessage errorMessage =
  do
    tupleFn <- loop id
    case tupleFn (Nothing, Nothing) of
      (Just v1, Just v2) -> return (errorMessage v1 v2)
      _ -> failure "Some of the error fields are missing"
  where
    loop state =
      (noticeField fieldState >>= id >>= loop) <|> pure state
      where
        fieldState =
          \case
            CodeNoticeFieldType -> \payload -> pure (state . (\(v1, v2) -> (Just payload, v2)))
            MessageNoticeFieldType -> \payload -> pure (state . (\(v1, v2) -> (v1, Just payload)))
            _ -> \_ -> pure state

{-# INLINE noticeField #-}
noticeField :: (NoticeFieldType -> ByteString -> a) -> BinaryParser a
noticeField cont =
  cont <$> noticeFieldType <*> nullTerminatedString

{-# INLINE noticeFieldType #-}
noticeFieldType :: BinaryParser NoticeFieldType
noticeFieldType =
  NoticeFieldType <$> word8

{-# INLINE nullTerminatedString #-}
nullTerminatedString :: BinaryParser ByteString
nullTerminatedString =
  bytesWhile (/= 0) <* byte

{-# INLINE protocolVersion #-}
protocolVersion :: BinaryParser (Word16, Word16)
protocolVersion =
  (,) <$> word16 <*> word16

{-# INLINE authenticationMessage #-}
authenticationMessage :: BinaryParser AuthenticationMessage
authenticationMessage =
  do
    method <- word32
    case method of
      0 -> return OkAuthenticationMessage
      3 -> return ClearTextPasswordAuthenticationMessage
      5 -> MD5PasswordAuthenticationMessage <$> remainders
      _ -> failure ("Unsupported authentication method: " <> (fromString . show) method)

{-# INLINE notificationMessage #-}
notificationMessage :: (Word32 -> ByteString -> ByteString -> result) -> BinaryParser result
notificationMessage cont =
  cont <$> word32 <*> nullTerminatedString <*> nullTerminatedString

{-# INLINE dataRowMessage #-}
dataRowMessage :: (Word16 -> BinaryParser a) -> BinaryParser a
dataRowMessage contentsParser =
  do
    amountOfColumns <- word16
    contentsParser amountOfColumns

{-|
ParameterStatus (B)
Byte1('S')
Identifies the message as a run-time parameter status report.

Int32
Length of message contents in bytes, including self.

String
The name of the run-time parameter being reported.

String
The current value of the parameter.
-}
{-# INLINE parameterStatusMessagePayloadKeyValue #-}
parameterStatusMessagePayloadKeyValue :: (ByteString -> ByteString -> a) -> BinaryParser a
parameterStatusMessagePayloadKeyValue cont =
  cont <$> nullTerminatedString <*> nullTerminatedString
