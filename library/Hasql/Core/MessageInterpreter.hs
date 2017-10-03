module Hasql.Core.MessageInterpreter where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.MessageTypePredicates as G
import qualified Hasql.Protocol.Decoding as E
import qualified BinaryParser as D


{-|
Interpreter of a single message.
-}
newtype MessageInterpreter = MessageInterpreter (Word8 -> Maybe (ByteString -> IO ()))

instance Semigroup MessageInterpreter where
  {-# INLINE (<>) #-}
  (<>) (MessageInterpreter left) (MessageInterpreter right) =
    MessageInterpreter merged
    where
      merged type_ =
        case left type_ of
          Just parser -> Just parser
          Nothing -> case right type_ of
            Just parser -> Just parser
            Nothing -> Nothing

instance Monoid MessageInterpreter where
  mempty = MessageInterpreter (const Nothing)
  mappend = (<>)

{-# INLINE handler #-}
handler :: (Word8 -> Bool) -> (ByteString -> IO ()) -> MessageInterpreter
handler predicate handler =
  MessageInterpreter (\type_ -> if predicate type_ then Just handler else Nothing)

{-# INLINE parser #-}
parser :: (Word8 -> Bool) -> D.BinaryParser parsed -> (parsed -> IO ()) -> (Text -> IO ()) -> MessageInterpreter
parser predicate parser parsedHandler parsingErrorHandler =
  handler predicate (either parsingErrorHandler parsedHandler . D.run parser)

{-# INLINE ignored #-}
ignored :: (Word8 -> Bool) -> IO () -> MessageInterpreter
ignored predicate successHandler =
  handler predicate (const successHandler)

{-# INLINE error #-}
error :: (ByteString -> ByteString -> IO ()) -> (Text -> IO ()) -> MessageInterpreter
error parsedHandler =
  parser G.error (E.errorMessage parsedHandler) id

{-# INLINE dataRow #-}
dataRow :: D.BinaryParser row -> (row -> IO ()) -> (Text -> IO ()) -> MessageInterpreter
dataRow =
  parser G.dataRow

{-# INLINE ignoredDataRow #-}
ignoredDataRow :: IO () -> MessageInterpreter
ignoredDataRow =
  ignored G.dataRow

{-# INLINE commandComplete #-}
commandComplete :: (Int -> IO ()) -> (Text -> IO ()) -> MessageInterpreter
commandComplete =
  parser G.commandComplete E.commandCompleteMessageAffectedRows

{-# INLINE ignoredCommandComplete #-}
ignoredCommandComplete :: IO () -> MessageInterpreter
ignoredCommandComplete =
  ignored G.commandComplete

{-# INLINE bindComplete #-}
bindComplete :: IO () -> MessageInterpreter
bindComplete =
  ignored G.bindComplete

{-# INLINE parseComplete #-}
parseComplete :: IO () -> MessageInterpreter
parseComplete =
  ignored G.parseComplete

{-# INLINE readyForQuery #-}
readyForQuery :: IO () -> MessageInterpreter
readyForQuery =
  ignored G.readyForQuery

{-# INLINE emptyQuery #-}
emptyQuery :: IO () -> MessageInterpreter
emptyQuery =
  ignored G.emptyQuery

{-# INLINE portalSuspended #-}
portalSuspended :: IO () -> MessageInterpreter
portalSuspended =
  ignored G.portalSuspended
