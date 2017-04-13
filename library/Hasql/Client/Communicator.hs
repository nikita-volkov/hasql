module Hasql.Client.Communicator where

import Hasql.Prelude
import Hasql.Client.Model
import qualified Litsedey as A
import qualified BinaryParser as B
import qualified ByteString.StrictBuilder as L
import qualified Hasql.Protocol.Model as J
import qualified Hasql.Protocol.Encoding as K
import qualified Hasql.Client.Socket as F
import qualified Hasql.Client.Communicator.Actors.Interpreter as E
import qualified Hasql.Client.Communicator.Actors.Sender as F
import qualified Hasql.Client.Communicator.Actors.Receiver as G
import qualified Hasql.Client.MessagesConsumer as H


data Communicator =
  Communicator {
    interpreter :: A.Actor E.Message,
    sender :: A.Actor F.Message,
    receiver :: A.Actor G.Message
  }

acquire :: F.Socket -> IO Communicator
acquire socket =
  do
    sender <- F.actor socket
    interpreter <- E.actor sender
    receiver <- G.actor socket interpreter
    A.tell receiver G.ReceiveForeverMessage
    return (Communicator interpreter sender receiver)

sendAndConsume :: Communicator -> L.Builder -> H.MessagesConsumer result -> IO (IO (Either Error result))
sendAndConsume (Communicator interpreter sender receiver) messageBuilder (H.MessagesConsumer createConsumer) =
  do
    traceMarkerIO ("sendAndConsume")
    (messageInterpreter, failer, blocker) <- createConsumer
    A.tell interpreter (E.SendAndAggregateMessage (L.builderBytes messageBuilder) messageInterpreter failer)
    return blocker

send :: Communicator -> L.Builder -> (Text -> IO ()) -> IO ()
send (Communicator interpreter sender receiver) messageBuilder transportErrorHandler =
  A.tell sender (F.SendMessage messageBytes transportErrorHandler)
  where
    messageBytes =
      L.builderBytes messageBuilder

startUp :: Communicator -> ByteString -> Maybe ByteString -> Maybe ByteString -> [(ByteString, ByteString)] -> IO (IO (Either Error BackendSettings))
startUp communicator username passwordMaybe databaseMaybe runtimeParameters =
  do
    traceMarkerIO ("startUp")
    sendAndConsume communicator message consumer
  where
    message =
      K.startUpMessage 3 0 username databaseMaybe runtimeParameters
    consumer =
      H.startUp clearTextPasswordHandler md5PasswordHandler
      where
        clearTextPasswordHandler transportErrorHandler =
          send communicator message transportErrorHandler
          where
            message =
              K.clearTextPasswordMessage (fold passwordMaybe)
        md5PasswordHandler transportErrorHandler salt =
          send communicator message transportErrorHandler
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

release :: Communicator -> IO ()
release (Communicator interpreter sender receiver) =
  do
    A.tell sender (F.SendMessage (L.builderBytes K.terminateMessage) (const (return ())))
    A.kill sender
    A.kill interpreter
    A.kill receiver
