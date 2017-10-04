module Hasql.Core.Dispatcher where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Socket as A
import qualified ByteString.StrictBuilder as B
import qualified BinaryParser as D
import qualified Hasql.Core.ParseMessageStream as E
import qualified Hasql.Core.Loops.Serializer as SerializerLoop
import qualified Hasql.Core.Loops.Receiver as ReceiverLoop
import qualified Hasql.Core.Loops.Sender as SenderLoop
import qualified Hasql.Core.Loops.IncomingMessagesSlicer as IncomingMessagesSlicerLoop
import qualified Hasql.Core.Loops.Interpreter as InterpreterLoop


startDispatching :: A.Socket -> (Either Error Notification -> IO ()) -> IO (IO ())
startDispatching socket sendErrorOrNotification =
  do
    outgoingBytesQueue <- newTQueueIO
    incomingBytesQueue <- newTQueueIO
    serializerMessageQueue <- newTQueueIO
    incomingMessageQueue <- newTQueueIO
    resultProcessorQueue <- newTQueueIO
    errorVar <- newEmptyTMVarIO
    let
      loopSending =
        SenderLoop.loop socket
          (atomically (readTQueue outgoingBytesQueue))
          (atomically . putTMVar errorVar . TransportError)
      loopReceiving =
        ReceiverLoop.loop socket
          (atomically . writeTQueue incomingBytesQueue)
          (atomically . putTMVar errorVar . TransportError)
      loopSerializing =
        SerializerLoop.loop
          (atomically (readTQueue serializerMessageQueue))
          (atomically . writeTQueue outgoingBytesQueue)
      loopSlicing =
        IncomingMessagesSlicerLoop.loop
          (atomically (readTQueue incomingBytesQueue))
          (atomically . writeTQueue incomingMessageQueue)
      loopInterpreting =
        InterpreterLoop.loop
          (atomically (readTQueue incomingMessageQueue))
          (atomically (tryReadTQueue resultProcessorQueue))
          (\case
            InterpreterLoop.NotificationUnaffiliatedResult notification ->
              sendErrorOrNotification (Right notification)
            InterpreterLoop.ErrorMessageUnaffiliatedResult (ErrorMessage state details) ->
              sendErrorOrNotification (Left (BackendError state details))
            InterpreterLoop.ProtocolErrorUnaffiliatedResult details ->
              sendErrorOrNotification (Left (ProtocolError details)))
      in startThreads [loopInterpreting, loopSerializing, loopSlicing, loopSending, loopReceiving]
