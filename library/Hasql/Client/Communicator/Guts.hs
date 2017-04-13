module Hasql.Client.Communicator.Guts where

import Hasql.Prelude
import Hasql.Client.Model
import qualified Hasql.Client.Communicator.Receiver as A
import qualified Hasql.Client.Communicator.Sender as G
import qualified Hasql.Client.Socket as F
import qualified Hasql.Buffer as E
import qualified Hasql.Protocol.Model as J
import qualified Hasql.Protocol.Interpreter as B
import qualified ByteString.StrictBuilder as D


{-|
A receiving loop.
Quits in case of Transport or Protocol error or when Maybe is transmitted through the handler chan.
-}
runReceivingLoop :: F.Socket -> TQueue (Maybe (B.Interpreter, Error -> IO ())) -> IO ()
runReceivingLoop socket handlerChan =
  do
    receiver <- A.acquire socket
    fix $ \processNextInterpreter -> do
      readChanResult <- atomically (readTQueue handlerChan)
      forM_ readChanResult $ \(B.Interpreter sendMessage, sendError) -> do
        fix $ \processNextMessage -> {-# SCC "runReceivingLoop/processNextMessage" #-} do
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
  FlushSenderMessage (Either Text () -> IO ()) |
  TerminateSenderMessage

runSendingLoop :: F.Socket -> TQueue SenderMessage -> IO ()
runSendingLoop socket messageChan =
  do
    sender <- G.acquire socket
    fix $ \processNextMessage -> {-# SCC "runSendingLoop/processNextMessage" #-} do
      message <- atomically (readTQueue messageChan)
      case message of
        ScheduleSenderMessage builder ->
          G.schedule sender builder >> processNextMessage
        FlushSenderMessage handler ->
          G.flush sender >>= handler >> processNextMessage
        TerminateSenderMessage ->
          return ()
