module Hasql.Core.Protocol.Take where

import Hasql.Prelude
import Hasql.Core.Model
import Ptr.Take
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as A
import qualified Data.Vector as D
import qualified Hasql.Core.MessageTypePredicates as C
import qualified Hasql.Core.NoticeFieldTypes as E
import qualified Hasql.Core.ParseDataRow as F



-- * Postgres Protocol-specific Primitives
-------------------------

{-|
Int32
The length of the column value, in bytes (this count does not include itself). Can be zero. As a special case, -1 indicates a NULL column value. No value bytes follow in the NULL case.

Byten
The value of the column, in the format indicated by the associated format code. n is the above length.
-}
{-# INLINE sizedBytes #-}
sizedBytes :: Take (Maybe ByteString)
sizedBytes =
  {-# SCC "sizedBytes" #-} 
  do
    size <- fromIntegral <$> beWord32
    if size == -1
      then return Nothing
      else do
        !bytes_ <- bytes size
        return (Just bytes_)


-- * Responses
-------------------------

{-# INLINE responseBody #-}
responseBody :: Word8 -> Take (Maybe (Either Text Response))
responseBody type_ =
  {-# SCC "responseBody" #-} 
  if
    | C.dataRow type_ -> (Just . Right . DataRowResponse) <$> allBytes
    | C.commandComplete type_ -> commandCompleteBody (Just . Right . CommandCompleteResponse)
    | C.readyForQuery type_ -> fmap (Just . fmap ReadyForQueryResponse) readyForQueryBody
    | C.parseComplete type_ -> pure (Just (Right ParseCompleteResponse))
    | C.bindComplete type_ -> pure (Just (Right BindCompleteResponse))
    | C.emptyQuery type_ -> pure (Just (Right EmptyQueryResponse))
    | C.notification type_ -> Just . Right <$> notificationBody NotificationResponse
    | C.error type_ -> Just <$> errorResponseBody ErrorResponse
    | C.authentication type_ -> fmap (Just . fmap AuthenticationResponse) authenticationBody
    | C.parameterStatus type_ -> Just . Right <$> parameterStatusBody ParameterStatusResponse
    | True -> pure Nothing

{-# INLINE unparsedFieldsOfDataRowBody #-}
unparsedFieldsOfDataRowBody :: Take (Vector (Maybe ByteString))
unparsedFieldsOfDataRowBody =
  {-# SCC "unparsedFieldsOfDataRowBody" #-} 
  do
    amountOfColumns <- beWord16
    D.replicateM (fromIntegral amountOfColumns) sizedBytes

{-# INLINE dataRowBody #-}
dataRowBody :: F.ParseDataRow row -> Take (Either Text row)
dataRowBody (F.ParseDataRow rowLength vectorFn) =
  fmap fromUnparsedValues unparsedFieldsOfDataRowBody
  where
    fromUnparsedValues unparsedValues =
      if actualRowLength == rowLength
        then vectorFn unparsedValues 0
        else Left (fromString
          (showString "Invalid amount of columns: "
            (shows actualRowLength
              (showString ", expecting "
                (show rowLength)))))
      where
        actualRowLength =
          D.length unparsedValues

{-# INLINE commandCompleteBody #-}
commandCompleteBody :: (Int -> result) -> Take result
commandCompleteBody result =
  {-# SCC "commandCompleteBody" #-} 
  do
    header <- bytesWhile byteIsUpperLetter
    word8
    count <- case header of
      "INSERT" -> skipWhile byteIsDigit *> word8 *> unsignedASCIIIntegral <* word8
      _ -> unsignedASCIIIntegral <* word8
    return (result count)
  where
    byteIsUpperLetter byte =
      byte - 65 <= 25
    byteIsDigit byte =
      byte - 48 <= 9

{-# INLINE readyForQueryBody #-}
readyForQueryBody :: Take (Either Text TransactionStatus)
readyForQueryBody =
  {-# SCC "readyForQueryBody" #-} 
  do
    statusByte <- word8
    case statusByte of
      73 -> return (Right IdleTransactionStatus)
      84 -> return (Right ActiveTransactionStatus)
      69 -> return (Right FailedTransactionStatus)
      _ -> return (Left ("Unexpected transaction status byte: " <> (fromString . show) statusByte))

{-# INLINE notificationBody #-}
notificationBody :: (Word32 -> ByteString -> ByteString -> result) -> Take result
notificationBody result =
  {-# SCC "notificationBody" #-} 
  result <$> beWord32 <*> nullTerminatedBytes <*> nullTerminatedBytes

{-# INLINE errorResponseBody #-}
errorResponseBody :: (ByteString -> ByteString -> result) -> Take (Either Text result)
errorResponseBody result =
  {-# SCC "errorResponseBody" #-} 
  iterate Nothing Nothing
  where
    iterate code message =
      element <|> end
      where
        element =
          join (noticeField (\type_ payload ->
            if
              | type_ == E.code -> iterate (Just payload) message
              | type_ == E.message -> iterate code (Just payload)
              | True -> iterate code message))
        end =
          do
            word8
            case code of
              Just code -> case message of
                Just message -> return (Right (result code message))
                _ -> return (Left "The \"message\" field is missing")
              _ -> return (Left "The \"code\" field is missing")

{-# INLINE noticeField #-}
noticeField :: (Word8 -> ByteString -> result) -> Take result
noticeField result =
  {-# SCC "noticeField" #-} 
  result <$> word8 <*> nullTerminatedBytes

{-# INLINE authenticationBody #-}
authenticationBody :: Take (Either Text AuthenticationStatus)
authenticationBody =
  {-# SCC "authenticationBody" #-} 
  do
    status <- beWord32
    case status of
      0 -> return (Right OkAuthenticationStatus)
      3 -> return (Right NeedClearTextPasswordAuthenticationStatus)
      5 -> do
        salt <- bytes 4
        return (Right (NeedMD5PasswordAuthenticationStatus salt))
      _ -> return (Left ("Unsupported authentication status: " <> (fromString . show) status))

{-# INLINE parameterStatusBody #-}
parameterStatusBody :: (ByteString -> ByteString -> result) -> Take result
parameterStatusBody result =
  {-# SCC "parameterStatusBody" #-} 
  result <$> nullTerminatedBytes <*> nullTerminatedBytes
