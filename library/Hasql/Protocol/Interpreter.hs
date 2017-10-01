module Hasql.Protocol.Interpreter where

import Hasql.Prelude
import Hasql.Protocol.Model
import qualified BinaryParser as A
import qualified Hasql.Protocol.Decoding as D
import qualified Hasql.Protocol.MessagePayloadInterpretation as B


{-|
Composable interpreter of backend messages.
-}
newtype Interpreter =
  {-|
  * A function, which processes the backend message type and
    its payload, signaling whether it expects more messages with 'True'.
  -}
  Interpreter (MessageType -> ByteString -> IO Bool)

instance Semigroup Interpreter where
  {-# INLINE (<>) #-}
  (<>) (Interpreter leftFn) (Interpreter rightFn) =
    Interpreter $ \messageType messageBytes ->
      (||) <$> leftFn messageType messageBytes <*> rightFn messageType messageBytes

instance Monoid Interpreter where
  {-# INLINE mempty #-}
  mempty =
    Interpreter (const (const (return False)))
  {-# INLINE mappend #-}
  mappend =
    (<>)

rowsReduction ::
  A.BinaryParser row ->
  FoldM IO row result ->
  (result -> IO ()) {-^ Result handler -} ->
  (Text -> IO ()) {-^ Row parsing error -}->
  (ByteString -> ByteString -> IO ()) {-^ Backend error handler -} ->
  (Text -> IO ()) {-^ Protocol error handler -} ->
  IO Interpreter
rowsReduction rowParser (FoldM progress enter exit) resultHandler rowParsingErrorHandler backendErrorHandler protocolErrorHandler =
  interpreter <$> (newIORef =<< enter)
  where
    interpreter accumulatorRef =
      Interpreter def
      where
        def messageType messageBytes =
          case messageType of
            DataRowMessageType ->
              do
                case {-# SCC "rowsReduction/rowParser" #-} A.run (rowParser <* A.endOfInput) messageBytes of
                  Right parsedRow ->
                    do
                      accumulator <- readIORef accumulatorRef
                      newAccumulator <- progress accumulator parsedRow
                      writeIORef accumulatorRef newAccumulator
                  Left rowParsingError ->
                    rowParsingErrorHandler rowParsingError
                return True
            CommandCompleteMessageType ->
              do
                accumulator <- readIORef accumulatorRef
                result <- exit accumulator
                resultHandler result
                return False
            ErrorMessageType ->
              do
                case A.run D.errorMessage messageBytes of
                  Right (Error code message) ->
                    backendErrorHandler code message
                  Left parsingError ->
                    protocolErrorHandler ("ErrorResponse parsing error: " <> parsingError)
                return False
            EmptyQueryMessageType ->
              return False
            PortalSuspendedMessageType ->
              return False
            _ ->
              return True

rowsAffected ::
  (Int -> IO ()) {-^ Result handler -} ->
  (ByteString -> ByteString -> IO ()) {-^ Backend error handler -} ->
  (Text -> IO ()) {-^ Protocol error handler -} ->
  Interpreter
rowsAffected resultHandler backendErrorHandler protocolErrorHandler =
  Interpreter $ \case
    CommandCompleteMessageType -> \messageBytes ->
      do
        case A.run D.commandCompleteMessageAffectedRows messageBytes of
          Right rowsAffected ->
            resultHandler rowsAffected
          Left parsingError ->
            protocolErrorHandler ("CommandComplete parsing error: " <> parsingError)
        return False
    ErrorMessageType -> \messageBytes ->
      do
        case A.run D.errorMessage messageBytes of
          Right (Error code message) ->
            backendErrorHandler code message
          Left parsingError ->
            protocolErrorHandler ("ErrorResponse parsing error: " <> parsingError)
        return False
    EmptyQueryMessageType ->
      const (return False)
    PortalSuspendedMessageType ->
      const (return False)
    _ ->
      const (return True)

error ::
  (ByteString -> ByteString -> IO ()) {-^ Backend error handler -} ->
  (Text -> IO ()) {-^ Protocol error handler -} ->
  Interpreter
error backendErrorHandler protocolErrorHandler =
  Interpreter $ \case
    ErrorMessageType ->
      \messageBytes ->
      B.error backendErrorHandler protocolErrorHandler messageBytes $> False
    _ ->
      const (return True)

bindComplete ::
  (IO ()) ->
  (ByteString -> ByteString -> IO ()) {-^ Backend error handler -} ->
  (Text -> IO ()) {-^ Protocol error handler -} ->
  Interpreter
bindComplete bindCompleteHandler backendErrorHandler protocolErrorHandler =
  Interpreter $ \case
    BindCompleteMessageType ->
      const (bindCompleteHandler $> False)
    ErrorMessageType ->
      \messageBytes ->
      B.error backendErrorHandler protocolErrorHandler messageBytes $> False
    _ ->
      const (return True)

parseComplete ::
  (IO ()) ->
  (ByteString -> ByteString -> IO ()) {-^ Backend error handler -} ->
  (Text -> IO ()) {-^ Protocol error handler -} ->
  Interpreter
parseComplete parseCompleteHandler backendErrorHandler protocolErrorHandler =
  Interpreter $ \case
    ParseCompleteMessageType ->
      const (parseCompleteHandler $> False)
    ErrorMessageType ->
      \messageBytes ->
      B.error backendErrorHandler protocolErrorHandler messageBytes $> False
    _ ->
      const (return True)

readyForQuery ::
  (IO ()) ->
  (ByteString -> ByteString -> IO ()) {-^ Backend error handler -} ->
  (Text -> IO ()) {-^ Protocol error handler -} ->
  Interpreter
readyForQuery readyForQueryHandler backendErrorHandler protocolErrorHandler =
  Interpreter $ \case
    ReadyForQueryMessageType ->
      const (readyForQueryHandler $> False)
    ErrorMessageType ->
      \messageBytes ->
      B.error backendErrorHandler protocolErrorHandler messageBytes $> False
    _ ->
      const (return True)
