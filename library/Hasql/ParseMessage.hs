module Hasql.ParseMessage where

import Hasql.Prelude
import Hasql.Model hiding (Error(..))
import qualified Hasql.Choosing as C
import qualified Hasql.MessageTypePredicates as G
import qualified Hasql.ParseDataRow as F
import qualified Hasql.Protocol.Decoding as E
import qualified Hasql.Protocol.Model as A
import qualified BinaryParser as D


newtype ParseMessage result =
  ParseMessage (C.Choosing Word8 (ReaderT ByteString (Either Error)) result)
  deriving (Functor, Applicative, Alternative)

data Error =
  ParsingError !Text !Text
  deriving (Show, Eq)

run :: ParseMessage result -> Word8 -> Maybe (ByteString -> Either Error result)
run (ParseMessage (C.Choosing typeFn)) type_ =
  case typeFn type_ of
    Just (ReaderT payloadFn) -> Just (payloadFn)
    Nothing -> Nothing

{-# INLINE payloadFn #-}
payloadFn :: (Word8 -> Bool) -> (ByteString -> Either Error result) -> ParseMessage result
payloadFn predicate payloadFn =
  ParseMessage (C.Choosing (bool Nothing (Just (ReaderT payloadFn)) . predicate))

{-# INLINE payloadParser #-}
payloadParser :: (Word8 -> Bool) -> Text -> D.BinaryParser parsed -> ParseMessage parsed
payloadParser predicate context parser =
  payloadFn predicate (either (Left . ParsingError context) Right . D.run parser)

{-# INLINE withoutPayload #-}
withoutPayload :: (Word8 -> Bool) -> ParseMessage ()
withoutPayload predicate =
  payloadFn predicate (const (Right ()))

{-# INLINE error #-}
error :: ParseMessage ErrorMessage
error =
  payloadParser G.error "ErrorResponse" (E.errorMessage ErrorMessage)

{-# INLINE errorCont #-}
errorCont :: (ByteString -> ByteString -> result) -> ParseMessage result
errorCont message =
  payloadParser G.error "ErrorResponse" (E.errorMessage message)

{-# INLINE notification #-}
notification :: ParseMessage Notification
notification =
  payloadParser G.notification "NotificationResponse" (E.notificationMessage Notification)

{-# INLINE dataRow #-}
dataRow :: F.ParseDataRow row -> ParseMessage row
dataRow =
  payloadParser G.dataRow "DataRow" . E.parseDataRow

{-# INLINE dataRowWithoutData #-}
dataRowWithoutData :: ParseMessage ()
dataRowWithoutData =
  withoutPayload G.dataRow

{-# INLINE commandComplete #-}
commandComplete :: ParseMessage Int
commandComplete =
  payloadParser G.commandComplete "CommandComplete" E.commandCompleteMessageAffectedRows

{-# INLINE commandCompleteWithoutAmount #-}
commandCompleteWithoutAmount :: ParseMessage ()
commandCompleteWithoutAmount =
  withoutPayload G.commandComplete

{-# INLINE bindComplete #-}
bindComplete :: ParseMessage ()
bindComplete =
  withoutPayload G.bindComplete

{-# INLINE parseComplete #-}
parseComplete :: ParseMessage ()
parseComplete =
  withoutPayload G.parseComplete

{-# INLINE readyForQuery #-}
readyForQuery :: ParseMessage ()
readyForQuery =
  withoutPayload G.readyForQuery

{-# INLINE emptyQuery #-}
emptyQuery :: ParseMessage ()
emptyQuery =
  withoutPayload G.emptyQuery

{-# INLINE portalSuspended #-}
portalSuspended :: ParseMessage ()
portalSuspended =
  withoutPayload G.portalSuspended

{-# INLINE authentication #-}
authentication :: ParseMessage A.AuthenticationMessage
authentication =
  payloadParser G.authentication "AuthenticationMessage" E.authenticationMessage

{-# INLINE parameterStatus #-}
parameterStatus :: ParseMessage (ByteString, ByteString)
parameterStatus =
  payloadParser G.parameterStatus "ParameterStatus" (E.parameterStatusMessagePayloadKeyValue (,))
