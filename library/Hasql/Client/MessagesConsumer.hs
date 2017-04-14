module Hasql.Client.MessagesConsumer
where

import Hasql.Prelude
import Hasql.Client.Model
import qualified BinaryParser as A
import qualified Hasql.Protocol.Decoding as D
import qualified Hasql.Protocol.Interpreter as H
import qualified Hasql.Protocol.MessagePayloadInterpretation as B
import qualified Hasql.Protocol.Model as J
import qualified Hasql.Client.EventBasedResultAggregation as C
import qualified Control.Foldl as I


{-|
Interpreter of a whole phase of communication.
E.g., post-startup or data-rows retrieval.
-}
newtype MessagesConsumer result =
  {-|
  * Message interpreter and an IO action producting the result.
  -}
  MessagesConsumer (IO (H.Interpreter, Error -> IO (), IO (Either Error result)))

deriving instance Functor MessagesConsumer

instance Applicative MessagesConsumer where
  {-# INLINE pure #-}
  pure result =
    MessagesConsumer (pure (mempty, mempty, pure (pure result)))
  {-# INLINE (<*>) #-}
  (<*>) (MessagesConsumer leftIO) (MessagesConsumer rightIO) =
    MessagesConsumer $ do
      (leftInterpreter, leftFailer, leftResultIO) <- leftIO
      (rightInterpreter, rightFailer, rightResultIO) <- rightIO
      let
        interpreter =
          leftInterpreter <> rightInterpreter
        failer =
          -- Using "mappend", because there's no Semigroup
          mappend leftFailer rightFailer
        resultIO =
          (<*>) <$> leftResultIO <*> rightResultIO
        in return (interpreter, failer, resultIO)

{-# INLINE unblockingInterpreter #-}
unblockingInterpreter :: ((Either Error result -> IO ()) -> H.Interpreter) -> MessagesConsumer result
unblockingInterpreter interpreter =
  MessagesConsumer $ do
    outputMVar <- newEmptyMVar
    let output = void . tryPutMVar outputMVar
    return (interpreter output, output . Left, takeMVar outputMVar)

{-# INLINE unblockingInterpreterIO #-}
unblockingInterpreterIO :: ((Either Error result -> IO ()) -> IO H.Interpreter) -> MessagesConsumer result
unblockingInterpreterIO interpreterIO =
  MessagesConsumer $ do
    outputMVar <- newEmptyMVar
    let
      output output =
        tryPutMVar outputMVar output $> ()
    interpreter <- interpreterIO output
    return (interpreter, output . Left, takeMVar outputMVar)

rowsAffected :: MessagesConsumer Int
rowsAffected =
  unblockingInterpreter interpreter
  where
    interpreter unblock =
      H.rowsAffected resultHandler backendErrorHandler protocolErrorHandler
      where
        resultHandler =
          unblock . Right
        backendErrorHandler code message =
          unblock (Left (BackendError code message))
        protocolErrorHandler =
          unblock . Left . ProtocolError

rowsReduction :: A.BinaryParser row -> FoldM IO row reduction -> MessagesConsumer reduction
rowsReduction rowParser rowFold =
  unblockingInterpreterIO interpreterIO
  where
    interpreterIO unblock =
      H.rowsReduction rowParser rowFold reductionHandler rowParsingErrorHandler backendErrorHandler protocolErrorHandler
      where
        reductionHandler =
          unblock . Right
        backendErrorHandler code message =
          unblock (Left (BackendError code message))
        protocolErrorHandler =
          unblock . Left . ProtocolError
        rowParsingErrorHandler =
          unblock . Left . DecodingError . mappend "Row: "

rowsReductionOnTheBlockedThread :: A.BinaryParser row -> FoldM IO row reduction -> MessagesConsumer reduction
rowsReductionOnTheBlockedThread rowParser rowFold =
  MessagesConsumer $ do
    (computeResult, sendEvent) <- C.rowsReduction rowParser rowFold
    let
      errorHandler =
        sendEvent . C.ErrorEvent
      backendErrorHandler code message =
        errorHandler (BackendError code message)
      protocolErrorHandler =
        errorHandler . ProtocolError
      interpreter =
        H.Interpreter $ {-# SCC "rowsReductionOnTheBlockedThread/interpreter" #-} \case
          J.DataRowMessageType ->
            \bytes -> sendEvent (C.DataRowEvent bytes) $> True
          J.CommandCompleteMessageType ->
            const (sendEvent C.FinishEvent $> False)
          J.ErrorMessageType ->
            fmap (const False) . B.errorResponse backendErrorHandler protocolErrorHandler
          J.EmptyQueryMessageType ->
            const (sendEvent C.FinishEvent $> False)
          J.PortalSuspendedMessageType ->
            const (protocolErrorHandler "Portal unexpectedly suspended" $> False)
          _ ->
            const (return True)
        in return (interpreter, errorHandler, computeResult)
{-|
Single row.
-}
row :: A.BinaryParser row -> MessagesConsumer row
row rowParser =
  unblockingInterpreterIO interpreterIO
  where
    interpreterIO unblock =
      H.rowsReduction rowParser rowFold reductionHandler rowParsingErrorHandler backendErrorHandler protocolErrorHandler
      where
        rowFold =
          I.generalize I.head
        reductionHandler =
          \case
            Just row -> unblock (Right row)
            Nothing -> unblock (Left (DecodingError ("No rows")))
        backendErrorHandler code message =
          unblock (Left (BackendError code message))
        protocolErrorHandler =
          unblock . Left . ProtocolError
        rowParsingErrorHandler =
          unblock . Left . DecodingError . mappend "Row: "

bindComplete :: MessagesConsumer ()
bindComplete =
  unblockingInterpreter interpreter
  where
    interpreter unblock =
      H.bindComplete
        (unblock (Right ()))
        (\code message -> unblock (Left (BackendError code message)))
        (unblock . Left . ProtocolError)

parseComplete :: MessagesConsumer ()
parseComplete =
  unblockingInterpreter interpreter
  where
    interpreter unblock =
      H.parseComplete
        (unblock (Right ()))
        (\code message -> unblock (Left (BackendError code message)))
        (unblock . Left . ProtocolError)

sync :: MessagesConsumer ()
sync =
  unblockingInterpreter interpreter
  where
    interpreter unblock =
      H.readyForQuery
        (unblock (Right ()))
        (\code message -> unblock (Left (BackendError code message)))
        (unblock . Left . ProtocolError)

startUp ::
  (IO (Either Error ())) {-^ ClearTextPassword handler -} ->
  (ByteString -> IO (Either Error ())) {-^ MD5 with salt handler -} ->
  MessagesConsumer BackendSettings
startUp clearTextPasswordHandler md5PasswordHandler =
  unblockingInterpreterIO $
  \unblock -> interpreter <$> newIORef Nothing <*> pure unblock
  where
    interpreter integerDateTimesMaybeRef unblock =
      H.Interpreter $ \case
        J.AuthenticationMessageType ->
          \messageBytes ->
          do
            B.authentication
              (return ())
              (clearTextPasswordHandler >>= either errorHandler return)
              (\salt -> md5PasswordHandler salt >>= either errorHandler return)
              (\error -> protocolErrorHandler error)
              messageBytes
            return True
        J.ParameterStatusMessageType ->
          \messageBytes ->
          B.parameterStatus parameterHandler protocolErrorHandler messageBytes $> True
        J.ReadyForQueryMessageType ->
          const $ do
            integerDateTimesMaybe <- readIORef integerDateTimesMaybeRef
            case BackendSettings <$> integerDateTimesMaybe of
              Just backendSettings ->
                unblock (Right backendSettings)
              Nothing ->
                unblock (Left (ProtocolError ("Missing required backend settings")))
            return False
        J.ErrorMessageType ->
          \messageBytes ->
          B.errorResponse backendErrorHandler protocolErrorHandler messageBytes $> False
        _ ->
          const (return True)
      where
        parameterHandler =
          \case
            "integer_datetimes" ->
              \case
                "on" ->
                  writeIORef integerDateTimesMaybeRef (Just True)
                "off" ->
                  writeIORef integerDateTimesMaybeRef (Just False)
                x ->
                  unblock (Left (ProtocolError ("Unexpected \"integer_datetimes\" value: " <> (fromString . show) x)))
            _ ->
              const (return ())
        errorHandler =
          unblock . Left
        backendErrorHandler code message =
          unblock (Left (BackendError code message))
        protocolErrorHandler message =
          unblock (Left (ProtocolError message))
        transportErrorHandler message =
          unblock (Left (TransportError message))
