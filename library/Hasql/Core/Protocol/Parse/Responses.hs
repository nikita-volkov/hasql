module Hasql.Core.Protocol.Parse.Responses where

import Hasql.Prelude hiding (fail)
import Hasql.Core.Protocol.Parse.Primitives
import Hasql.Core.Model
import Ptr.Parse
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as A
import qualified Data.Vector as D
import qualified Hasql.Core.MessageTypePredicates as C
import qualified Hasql.Core.NoticeFieldTypes as E
import qualified Hasql.Core.ParseDataRow as F


{-# INLINE responseBody #-}
responseBody :: Word8 -> Parse (Maybe Response)
responseBody type_ =
  {-# SCC "responseBody" #-} 
  if
    | C.dataRow type_ -> (Just . DataRowResponse) <$> allBytes
    | C.commandComplete type_ -> Just . CommandCompleteResponse <$> commandCompleteBody
    | C.readyForQuery type_ -> fmap (Just . ReadyForQueryResponse) readyForQueryBody
    | C.parseComplete type_ -> pure (Just ParseCompleteResponse)
    | C.bindComplete type_ -> pure (Just BindCompleteResponse)
    | C.emptyQuery type_ -> pure (Just EmptyQueryResponse)
    | C.notification type_ -> Just <$> notificationBody NotificationResponse
    | C.error type_ -> Just <$> errorResponseBody ErrorResponse
    | C.authentication type_ -> fmap (Just . AuthenticationResponse) authenticationBody
    | C.parameterStatus type_ -> Just <$> parameterStatusBody ParameterStatusResponse
    | True -> pure Nothing

{-# INLINE unparsedFieldsOfDataRowBody #-}
unparsedFieldsOfDataRowBody :: Parse (Vector (Maybe ByteString))
unparsedFieldsOfDataRowBody =
  {-# SCC "unparsedFieldsOfDataRowBody" #-} 
  do
    amountOfColumns <- beWord16
    D.replicateM (fromIntegral amountOfColumns) sizedBytes

{-# INLINE dataRowBody #-}
dataRowBody :: F.ParseDataRow row -> Parse row
dataRowBody (F.ParseDataRow rowLength parse) =
  do
    amountOfColumns <- beWord16
    if fromIntegral amountOfColumns == rowLength
      then parse
      else fail (fromString
        (showString "Invalid amount of columns: "
          (shows amountOfColumns
            (showString ", expecting "
              (show rowLength)))))

{-# INLINE commandCompleteBody #-}
commandCompleteBody :: Parse Int
commandCompleteBody =
  {-# SCC "commandCompleteBody" #-} 
  do
    header <- bytesWhile byteIsUpperLetter
    word8
    case header of
      "INSERT" -> skipWhile byteIsDigit *> word8 *> unsignedASCIIIntegral <* word8
      _ -> unsignedASCIIIntegral <* word8
  where
    byteIsUpperLetter byte =
      byte - 65 <= 25
    byteIsDigit byte =
      byte - 48 <= 9

{-# INLINE readyForQueryBody #-}
readyForQueryBody :: Parse TransactionStatus
readyForQueryBody =
  {-# SCC "readyForQueryBody" #-} 
  do
    statusByte <- word8
    case statusByte of
      73 -> return IdleTransactionStatus
      84 -> return ActiveTransactionStatus
      69 -> return FailedTransactionStatus
      _ -> fail ("Unexpected transaction status byte: " <> (fromString . show) statusByte)

{-# INLINE notificationBody #-}
notificationBody :: (Word32 -> ByteString -> ByteString -> result) -> Parse result
notificationBody result =
  {-# SCC "notificationBody" #-} 
  result <$> beWord32 <*> nullTerminatedBytes <*> nullTerminatedBytes

{-# INLINE errorResponseBody #-}
errorResponseBody :: (ByteString -> ByteString -> result) -> Parse result
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
                Just message -> return (result code message)
                _ -> fail "The \"message\" field is missing"
              _ -> fail "The \"code\" field is missing"

{-# INLINE noticeField #-}
noticeField :: (Word8 -> ByteString -> result) -> Parse result
noticeField result =
  {-# SCC "noticeField" #-} 
  result <$> word8 <*> nullTerminatedBytes

{-# INLINE authenticationBody #-}
authenticationBody :: Parse AuthenticationStatus
authenticationBody =
  {-# SCC "authenticationBody" #-} 
  do
    status <- beWord32
    case status of
      0 -> return OkAuthenticationStatus
      3 -> return NeedClearTextPasswordAuthenticationStatus
      5 -> do
        salt <- bytes 4
        return (NeedMD5PasswordAuthenticationStatus salt)
      _ -> fail ("Unsupported authentication status: " <> (fromString . show) status)

{-# INLINE parameterStatusBody #-}
parameterStatusBody :: (ByteString -> ByteString -> result) -> Parse result
parameterStatusBody result =
  {-# SCC "parameterStatusBody" #-} 
  result <$> nullTerminatedBytes <*> nullTerminatedBytes


