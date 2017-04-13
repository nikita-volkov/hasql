module Hasql.Client.Communicator
(
  Communicator,
  acquire,
  release,
  flush,
  startUp,
  sendAndConsume,
  parse,
  bind,
  bindEncoded,
  executeReducing,
  executeCounting,
  execute,
  sync,
)
where

import Hasql.Prelude
import Hasql.Client.Model
import qualified BinaryParser as B
import qualified ByteString.StrictBuilder as L
import qualified Hasql.Protocol.Model as J
import qualified Hasql.Protocol.Encoding as K
import qualified Hasql.Protocol.Interpreter as A
import qualified Hasql.Client.Socket as F
import qualified Hasql.Client.MessagesConsumer as H
import qualified Hasql.Client.Communicator.Guts as I
import qualified SlaveThread as C


data Communicator =
  Communicator {
    scheduleReceiver :: A.Interpreter -> (Error -> IO ()) -> IO (),
    scheduleMessage :: L.Builder -> IO (),
    flush :: IO (IO (Either Error ())),
    release :: IO ()
  }

acquire :: F.Socket -> IO Communicator
acquire socket =
  do
    receiverQueue <- newTQueueIO
    receiverThreadID <- C.fork (I.runReceivingLoop socket receiverQueue)
    labelThread receiverThreadID "Receiver"
    senderQueue <- newTQueueIO
    senderThreadID <- C.fork (I.runSendingLoop socket senderQueue)
    labelThread senderThreadID "Sender"
    let
      scheduleReceiver interpreter errorHandler =
        atomically (writeTQueue receiverQueue (Just (interpreter, errorHandler)))
      scheduleMessage builder =
        atomically (writeTQueue senderQueue (I.ScheduleSenderMessage builder))
      flush =
        do
          resultMVar <- newEmptyMVar
          atomically (writeTQueue senderQueue (I.FlushSenderMessage (putMVar resultMVar . first TransportError)))
          return (takeMVar resultMVar)
      release =
        do
          atomically (writeTQueue receiverQueue Nothing)
          atomically (writeTQueue senderQueue (I.ScheduleSenderMessage K.terminateMessage))
          atomically (writeTQueue senderQueue (I.FlushSenderMessage (const (return ()))))
          atomically (writeTQueue senderQueue (I.TerminateSenderMessage))
      in return (Communicator scheduleReceiver scheduleMessage flush release)

sendAndConsume :: Communicator -> L.Builder -> H.MessagesConsumer result -> IO (IO (Either Error result))
sendAndConsume communicator messageBuilder (H.MessagesConsumer createConsumer) =
  do
    (messageInterpreter, failer, blocker) <- createConsumer
    scheduleMessage communicator messageBuilder
    scheduleReceiver communicator messageInterpreter failer
    return blocker

sendAndFlush :: Communicator -> L.Builder -> IO (IO (Either Error ()))
sendAndFlush communicator messageBuilder =
  do
    scheduleMessage communicator messageBuilder
    flush communicator

startUp :: Communicator -> ByteString -> Maybe ByteString -> Maybe ByteString -> [(ByteString, ByteString)] -> IO (IO (Either Error BackendSettings))
startUp communicator username passwordMaybe databaseMaybe runtimeParameters =
  do
    (<*) <$> sendAndConsume communicator message consumer <*> flush communicator
  where
    message =
      K.startUpMessage 3 0 username databaseMaybe runtimeParameters
    consumer =
      H.startUp clearTextPasswordHandler md5PasswordHandler
      where
        clearTextPasswordHandler =
          join (sendAndFlush communicator message)
          where
            message =
              K.clearTextPasswordMessage (fold passwordMaybe)
        md5PasswordHandler salt =
          join (sendAndFlush communicator message)
          where
            message =
              K.md5PasswordMessage username (fold passwordMaybe) salt

{-|
Send a parse message and receive a future of result.
-}
parse :: Communicator -> ByteString -> ByteString -> Vector Word32 -> IO (IO (Either Error ()))
parse communicator preparedStatementName query oids =
  sendAndConsume communicator (K.parseMessage preparedStatementName query oids) H.parseComplete

bind :: Communicator -> ByteString -> ByteString -> Vector (Maybe L.Builder) -> IO (IO (Either Error ()))
bind communicator portalName preparedStatementName parameters =
  sendAndConsume communicator (K.binaryFormatBindMessage portalName preparedStatementName parameters) H.bindComplete

{-|
Bind a preencoded param array.
-}
bindEncoded :: Communicator -> ByteString -> ByteString -> Word16 -> L.Builder -> IO (IO (Either Error ()))
bindEncoded communicator portalName preparedStatementName paramsAmount encodedParams =
  sendAndConsume communicator message H.bindComplete
  where
    message =
      K.binaryFormatBindMessageWithEncodedParams portalName preparedStatementName paramsAmount encodedParams

executeReducing :: Communicator -> ByteString -> B.BinaryParser row -> FoldM IO row reduction -> IO (IO (Either Error reduction))
executeReducing communicator portalName rowParser rowReducer =
  sendAndConsume communicator (K.unlimitedExecuteMessage portalName) (H.rowsReduction rowParser rowReducer)

executeCounting :: Communicator -> ByteString -> IO (IO (Either Error Int))
executeCounting communicator portalName =
  sendAndConsume communicator (K.unlimitedExecuteMessage portalName) H.rowsAffected

execute :: Communicator -> ByteString -> H.MessagesConsumer result -> IO (IO (Either Error result))
execute communicator portalName =
  sendAndConsume communicator (K.unlimitedExecuteMessage portalName)

sync :: Communicator -> IO (IO (Either Error ()))
sync communicator =
  sendAndConsume communicator K.syncMessage H.sync
