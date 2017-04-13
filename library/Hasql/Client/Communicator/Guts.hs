module Hasql.Client.Communicator.Guts where

import Hasql.Prelude
import Hasql.Client.Model
import qualified Hasql.Client.Communicator.Receiver as A
import qualified Hasql.Client.Communicator.Sender as G
import qualified Hasql.Client.Socket as F
import qualified Hasql.Buffer as E
import qualified Hasql.Protocol.Model as J
import qualified Hasql.Protocol.Interpreter as B
import qualified Control.Concurrent.Chan.Unagi as C
import qualified ByteString.StrictBuilder as D


{-|
A receiving loop.
Quits in case of Transport or Protocol error or when Maybe is transmitted through the handler chan.
-}
runReceivingLoop :: F.Socket -> C.OutChan (Maybe (B.Interpreter, Error -> IO ())) -> IO ()
runReceivingLoop socket handlerChan =
  do
    receiver <- A.acquire socket
    fix $ \processNextInterpreter -> do
      readChanResult <- C.readChan handlerChan
      forM_ readChanResult $ \(B.Interpreter sendMessage, sendError) -> do
        fix $ \processNextMessage -> do
          getMessageResult <- A.use receiver (A.getMessage sendMessage)
          case getMessageResult of
            Right sendMessage ->
              do
                continue <- sendMessage
                if continue
                  then processNextMessage
                  else processNextInterpreter
            Left error ->
              case error of
                A.TransportError x ->
                  sendError (TransportError x)
                A.PeekingError x ->
                  sendError (ProtocolError x)

data SenderMessage =
  ScheduleSenderMessage D.Builder |
  FlushSenderMessage (Text -> IO ()) |
  TerminateSenderMessage

runSendingLoop :: F.Socket -> C.OutChan SenderMessage -> IO ()
runSendingLoop socket messageChan =
  do
    sender <- G.acquire socket
    fix $ \processNextMessage -> do
      message <- C.readChan messageChan
      case message of
        ScheduleSenderMessage builder ->
          G.schedule sender builder >> processNextMessage
        FlushSenderMessage errorHandler ->
          G.flush sender >>= either errorHandler return >> processNextMessage
        TerminateSenderMessage ->
          return ()
