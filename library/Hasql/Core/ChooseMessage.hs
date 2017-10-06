module Hasql.Core.ChooseMessage where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.MessageTypePredicates as G
import qualified Hasql.Protocol.Decoding as E
import qualified Hasql.Protocol.Model as A
import qualified BinaryParser as D


{-|
Interpreter of a single message.
-}
newtype ChooseMessage result = ChooseMessage (Word8 -> Maybe (ByteString -> result))

instance Functor ChooseMessage where
  {-# INLINE fmap #-}
  fmap mapping (ChooseMessage parse) =
    ChooseMessage (fmap (mapping .) . parse)

instance Applicative ChooseMessage where
  {-# INLINE pure #-}
  pure x =
    ChooseMessage (const (Just (const x)))
  {-# INLINE (<*>) #-}
  (<*>) (ChooseMessage left) (ChooseMessage right) =
    ChooseMessage $ \type_ ->
    case left type_ of
      Just leftPayloadFn -> case right type_ of
        Just rightPayloadFn -> Just $ \payload ->
          (leftPayloadFn payload) (rightPayloadFn payload)
        Nothing -> Nothing
      Nothing -> Nothing

instance Alternative ChooseMessage where
  {-# INLINE empty #-}
  empty =
    ChooseMessage (const Nothing)
  {-# INLINE (<|>) #-}
  (<|>) (ChooseMessage left) (ChooseMessage right) =
    ChooseMessage $ \type_ ->
    case left type_ of
      Just leftPayloadFn -> Just leftPayloadFn
      Nothing -> case right type_ of
        Just rightPayloadFn -> Just rightPayloadFn
        Nothing -> Nothing

{-# INLINE payloadFn #-}
payloadFn :: (Word8 -> Bool) -> (ByteString -> result) -> ChooseMessage result
payloadFn predicate payloadFn =
  ChooseMessage (\type_ -> if predicate type_ then Just payloadFn else Nothing)

{-# INLINE payloadParser #-}
payloadParser :: (Word8 -> Bool) -> D.BinaryParser parsed -> ChooseMessage (Either Text parsed)
payloadParser predicate parser =
  payloadFn predicate (D.run parser)

{-# INLINE withoutPayload #-}
withoutPayload :: (Word8 -> Bool) -> ChooseMessage ()
withoutPayload predicate =
  payloadFn predicate (const ())

{-# INLINE error #-}
error :: ChooseMessage (Either Text ErrorMessage)
error =
  payloadParser G.error (E.errorMessage ErrorMessage)

{-# INLINE errorCont #-}
errorCont :: (ByteString -> ByteString -> error) -> ChooseMessage (Either Text error)
errorCont message =
  payloadParser G.error (E.errorMessage message)

{-# INLINE notification #-}
notification :: ChooseMessage (Either Text Notification)
notification =
  payloadParser G.notification (E.notificationMessage Notification)

{-# INLINE dataRow #-}
dataRow :: D.BinaryParser row -> ChooseMessage (Either Text row)
dataRow =
  payloadParser G.dataRow

{-# INLINE dataRowWithoutData #-}
dataRowWithoutData :: ChooseMessage ()
dataRowWithoutData =
  withoutPayload G.dataRow

{-# INLINE commandComplete #-}
commandComplete :: ChooseMessage (Either Text Int)
commandComplete =
  payloadParser G.commandComplete E.commandCompleteMessageAffectedRows

{-# INLINE commandCompleteWithoutAmount #-}
commandCompleteWithoutAmount :: ChooseMessage ()
commandCompleteWithoutAmount =
  withoutPayload G.commandComplete

{-# INLINE bindComplete #-}
bindComplete :: ChooseMessage ()
bindComplete =
  withoutPayload G.bindComplete

{-# INLINE parseComplete #-}
parseComplete :: ChooseMessage ()
parseComplete =
  withoutPayload G.parseComplete

{-# INLINE readyForQuery #-}
readyForQuery :: ChooseMessage ()
readyForQuery =
  withoutPayload G.readyForQuery

{-# INLINE emptyQuery #-}
emptyQuery :: ChooseMessage ()
emptyQuery =
  withoutPayload G.emptyQuery

{-# INLINE portalSuspended #-}
portalSuspended :: ChooseMessage ()
portalSuspended =
  withoutPayload G.portalSuspended

{-# INLINE authentication #-}
authentication :: ChooseMessage (Either Text A.AuthenticationMessage)
authentication =
  payloadParser G.authentication E.authenticationMessage

{-# INLINE parameterStatus #-}
parameterStatus :: ChooseMessage (Either Text (ByteString, ByteString))
parameterStatus =
  payloadParser G.parameterStatus (E.parameterStatusMessagePayloadKeyValue (,))
