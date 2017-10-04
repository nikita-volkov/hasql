module Hasql.Core.ParseMessage where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.MessageTypePredicates as G
import qualified Hasql.Protocol.Decoding as E
import qualified BinaryParser as D


{-|
Interpreter of a single message.
-}
newtype ParseMessage result = ParseMessage (Word8 -> Maybe (ByteString -> result))

instance Functor ParseMessage where
  {-# INLINE fmap #-}
  fmap mapping (ParseMessage parse) =
    ParseMessage (fmap (mapping .) . parse)

instance Applicative ParseMessage where
  {-# INLINE pure #-}
  pure x =
    ParseMessage (const (Just (const x)))
  {-# INLINE (<*>) #-}
  (<*>) (ParseMessage left) (ParseMessage right) =
    ParseMessage $ \type_ ->
    case left type_ of
      Just leftPayloadFn -> case right type_ of
        Just rightPayloadFn -> Just $ \payload ->
          (leftPayloadFn payload) (rightPayloadFn payload)
        Nothing -> Nothing
      Nothing -> Nothing

instance Alternative ParseMessage where
  {-# INLINE empty #-}
  empty =
    ParseMessage (const Nothing)
  {-# INLINE (<|>) #-}
  (<|>) (ParseMessage left) (ParseMessage right) =
    ParseMessage $ \type_ ->
    case left type_ of
      Just leftPayloadFn -> Just leftPayloadFn
      Nothing -> case right type_ of
        Just rightPayloadFn -> Just rightPayloadFn
        Nothing -> Nothing

{-# INLINE payloadFn #-}
payloadFn :: (Word8 -> Bool) -> (ByteString -> result) -> ParseMessage result
payloadFn predicate payloadFn =
  ParseMessage (\type_ -> if predicate type_ then Just payloadFn else Nothing)

{-# INLINE payloadParser #-}
payloadParser :: (Word8 -> Bool) -> D.BinaryParser parsed -> ParseMessage (Either Text parsed)
payloadParser predicate parser =
  payloadFn predicate (D.run parser)

{-# INLINE withoutPayload #-}
withoutPayload :: (Word8 -> Bool) -> ParseMessage ()
withoutPayload predicate =
  payloadFn predicate (const ())

{-# INLINE error #-}
error :: ParseMessage (Either Text ErrorMessage)
error =
  payloadParser G.error (E.errorMessage ErrorMessage)

{-# INLINE notification #-}
notification :: ParseMessage (Either Text Notification)
notification =
  payloadParser G.notification (E.notificationMessage Notification)

{-# INLINE dataRow #-}
dataRow :: D.BinaryParser row -> ParseMessage (Either Text row)
dataRow =
  payloadParser G.dataRow

{-# INLINE dataRowWithoutData #-}
dataRowWithoutData :: ParseMessage ()
dataRowWithoutData =
  withoutPayload G.dataRow

{-# INLINE commandComplete #-}
commandComplete :: ParseMessage (Either Text Int)
commandComplete =
  payloadParser G.commandComplete E.commandCompleteMessageAffectedRows

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
