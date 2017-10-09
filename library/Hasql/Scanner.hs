module Hasql.Scanner where

import Hasql.Prelude
import Hasql.Model
import Scanner (Scanner)
import qualified Scanner as A
import qualified Data.ByteString as B
import qualified Data.Vector as D
import qualified Hasql.MessageTypePredicates as C
import qualified Hasql.NoticeFieldTypes as E


{-# INLINE word8 #-}
word8 :: Scanner Word8
word8 =
  A.anyWord8

{-# INLINE word16 #-}
word16 :: Scanner Word16
word16 =
  numOfSize 2

{-# INLINE word32 #-}
word32 :: Scanner Word32
word32 =
  numOfSize 4

{-# INLINE word64 #-}
word64 :: Scanner Word64
word64 =
  numOfSize 8

{-# INLINE numOfSize #-}
numOfSize :: (Bits a, Num a) => Int -> Scanner a
numOfSize size =
  B.foldl' (\n h -> shiftL n 8 .|. fromIntegral h) 0 <$> A.take size

{-# INLINE int32 #-}
int32 :: Scanner Int32
int32 =
  fromIntegral <$> word32

{-# INLINE messageTypeAndLength #-}
messageTypeAndLength :: (Word8 -> Word32 -> a) -> Scanner a
messageTypeAndLength cont =
  cont <$> word8 <*> payloadLength

{-# INLINE payloadLength #-}
payloadLength :: (Integral a, Bits a) => Scanner a
payloadLength =
  subtract 4 <$> numOfSize 4

{-# INLINE messageTypeAndPayload #-}
messageTypeAndPayload :: (Word8 -> ByteString -> a) -> Scanner a
messageTypeAndPayload cont =
  cont <$> word8 <*> (payloadLength >>= A.take)

-- |
-- Integral number encoded in ASCII.
{-# INLINE asciiIntegral #-}
asciiIntegral :: Integral a => Scanner a
asciiIntegral =
  B.foldl' step 0 <$> A.takeWhile byteIsDigit
  where
    byteIsDigit byte =
      byte - 48 <= 9
    step !state !byte =
      state * 10 + fromIntegral byte - 48

nullTerminatedString :: Scanner ByteString
nullTerminatedString =
  A.takeWhile (/= 0) <* A.anyWord8

-- * Responses
-------------------------

response :: Scanner (Maybe Response)
response =
  do
    type_ <- word8
    bodyLength <- payloadLength
    if
      | C.dataRow type_ -> dataRowBody (Just . DataRowResponse)
      | C.commandComplete type_ -> commandCompleteBody (Just . CommandCompleteResponse)
      | C.readyForQuery type_ -> readyForQueryBody (Just . ReadyForQueryResponse)
      | C.parseComplete type_ -> pure (Just ParseCompleteResponse)
      | C.emptyQuery type_ -> pure (Just EmptyQueryResponse)
      | C.notification type_ -> Just <$> notificationBody NotificationResponse
      | C.error type_ -> Just <$> errorResponseBody bodyLength ErrorResponse
      | C.authentication type_ -> Just <$> authenticationBody AuthenticationResponse
      | C.parameterStatus type_ -> Just <$> parameterStatusBody ParameterStatusResponse
      | True -> A.take bodyLength $> Nothing

dataRowBody :: (Vector (Maybe ByteString) -> result) -> Scanner result
dataRowBody result =
  do
    amountOfColumns <- word16
    bytesVector <- D.replicateM (fromIntegral amountOfColumns) sizedBytes
    return (result bytesVector)

commandCompleteBody :: (Int -> result) -> Scanner result
commandCompleteBody result =
  do
    header <- A.takeWhile byteIsUpperLetter
    A.anyWord8
    count <- case header of
      "INSERT" -> A.skipWhile byteIsDigit *> A.anyWord8 *> asciiIntegral <* A.anyWord8
      _ -> asciiIntegral <* A.anyWord8
    return (result count)
  where
    byteIsUpperLetter byte =
      byte - 65 <= 25
    byteIsDigit byte =
      byte - 48 <= 9

readyForQueryBody :: (TransactionStatus -> result) -> Scanner result
readyForQueryBody result =
  do
    statusByte <- A.anyWord8
    case statusByte of
      73 -> return (result IdleTransactionStatus)
      84 -> return (result ActiveTransactionStatus)
      69 -> return (result FailedTransactionStatus)
      _ -> fail (showString "Unexpected transaction status byte: " (show statusByte))

notificationBody :: (Word32 -> ByteString -> ByteString -> result) -> Scanner result
notificationBody result =
  result <$> word32 <*> nullTerminatedString <*> nullTerminatedString

errorResponseBody :: Int -> (ByteString -> ByteString -> result) -> Scanner result
errorResponseBody length result =
  do
    tuple <- iterate 0 Nothing Nothing
    case tuple of
      (Just code, Just message) -> return (result code message)
      _ -> fail "Some of the required error fields are missing"
  where
    iterate !offset code message  =
      if offset < length
        then join (noticeField (\type_ payload ->
          if
            | type_ == E.code -> iterate (2 + B.length payload) (Just payload) message
            | type_ == E.message -> iterate (2 + B.length payload) code (Just payload)
            | True -> iterate (2 + B.length payload) code message))
        else return (code, message)

noticeField :: (Word8 -> ByteString -> result) -> Scanner result
noticeField result =
  result <$> word8 <*> nullTerminatedString

authenticationBody :: (AuthenticationStatus -> result) -> Scanner result
authenticationBody result =
  do
    status <- word32
    case status of
      0 -> return (result OkAuthenticationStatus)
      3 -> return (result NeedClearTextPasswordAuthenticationStatus)
      5 -> do
        salt <- A.take 4
        return (result (NeedMD5PasswordAuthenticationStatus salt))
      _ -> fail ("Unsupported authentication status: " <> show status)

parameterStatusBody :: (ByteString -> ByteString -> result) -> Scanner result
parameterStatusBody result =
  result <$> nullTerminatedString <*> nullTerminatedString

{-|
Int32
The length of the column value, in bytes (this count does not include itself). Can be zero. As a special case, -1 indicates a NULL column value. No value bytes follow in the NULL case.

Byten
The value of the column, in the format indicated by the associated format code. n is the above length.
-}
sizedBytes :: Scanner (Maybe ByteString)
sizedBytes =
  do
    size <- fromIntegral <$> word32
    if size == -1
      then return Nothing
      else Just <$> A.take size
