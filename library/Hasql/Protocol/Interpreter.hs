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
  Interpreter (BackendMessageType -> ByteString -> IO Bool)

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
            DataRowBackendMessageType ->
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
            CommandCompleteBackendMessageType ->
              do
                accumulator <- readIORef accumulatorRef
                result <- exit accumulator
                resultHandler result
                return False
            ErrorBackendMessageType ->
              do
                case A.run D.errorMessage messageBytes of
                  Right (Error code message) ->
                    backendErrorHandler code message
                  Left parsingError ->
                    protocolErrorHandler ("ErrorResponse parsing error: " <> parsingError)
                return False
            EmptyQueryBackendMessageType ->
              return False
            PortalSuspendedBackendMessageType ->
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
    CommandCompleteBackendMessageType -> \messageBytes ->
      do
        case A.run D.commandCompleteMessageAffectedRows messageBytes of
          Right rowsAffected ->
            resultHandler rowsAffected
          Left parsingError ->
            protocolErrorHandler ("CommandComplete parsing error: " <> parsingError)
        return False
    ErrorBackendMessageType -> \messageBytes ->
      do
        case A.run D.errorMessage messageBytes of
          Right (Error code message) ->
            backendErrorHandler code message
          Left parsingError ->
            protocolErrorHandler ("ErrorResponse parsing error: " <> parsingError)
        return False
    EmptyQueryBackendMessageType ->
      const (return False)
    PortalSuspendedBackendMessageType ->
      const (return False)
    _ ->
      const (return True)

error ::
  (ByteString -> ByteString -> IO ()) {-^ Backend error handler -} ->
  (Text -> IO ()) {-^ Protocol error handler -} ->
  Interpreter
error backendErrorHandler protocolErrorHandler =
  Interpreter $ \case
    ErrorBackendMessageType ->
      \messageBytes ->
      B.errorResponse backendErrorHandler protocolErrorHandler messageBytes $> False
    _ ->
      const (return True)

bindComplete ::
  (IO ()) ->
  (ByteString -> ByteString -> IO ()) {-^ Backend error handler -} ->
  (Text -> IO ()) {-^ Protocol error handler -} ->
  Interpreter
bindComplete bindCompleteHandler backendErrorHandler protocolErrorHandler =
  Interpreter $ \case
    BindCompleteBackendMessageType ->
      const (bindCompleteHandler $> False)
    ErrorBackendMessageType ->
      \messageBytes ->
      B.errorResponse backendErrorHandler protocolErrorHandler messageBytes $> False
    _ ->
      const (return True)

parseComplete ::
  (IO ()) ->
  (ByteString -> ByteString -> IO ()) {-^ Backend error handler -} ->
  (Text -> IO ()) {-^ Protocol error handler -} ->
  Interpreter
parseComplete parseCompleteHandler backendErrorHandler protocolErrorHandler =
  Interpreter $ \case
    ParseCompleteBackendMessageType ->
      const (parseCompleteHandler $> False)
    ErrorBackendMessageType ->
      \messageBytes ->
      B.errorResponse backendErrorHandler protocolErrorHandler messageBytes $> False
    _ ->
      const (return True)

readyForQuery ::
  (IO ()) ->
  (ByteString -> ByteString -> IO ()) {-^ Backend error handler -} ->
  (Text -> IO ()) {-^ Protocol error handler -} ->
  Interpreter
readyForQuery readyForQueryHandler backendErrorHandler protocolErrorHandler =
  Interpreter $ \case
    ReadyForQueryBackendMessageType ->
      const (readyForQueryHandler $> False)
    ErrorBackendMessageType ->
      \messageBytes ->
      B.errorResponse backendErrorHandler protocolErrorHandler messageBytes $> False
    _ ->
      const (return True)
