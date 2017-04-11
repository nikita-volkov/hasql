module Hasql.Client.Communicator.Actors.Interpreter where

import Hasql.Prelude
import Hasql.Client.Model
import qualified BinaryParser as E
import qualified ByteString.StrictBuilder as L
import qualified Deque as B
import qualified Hasql.Protocol.Model as J
import qualified Hasql.Protocol.Encoding as K
import qualified Hasql.Protocol.Decoding as D
import qualified Hasql.Protocol.Interpreter as H
import qualified Hasql.Client.Communicator.Actors.Sender as C
import qualified Litsedey as A


data Message =
  {-| A message from server -}
  BackendMessageMessage !J.BackendMessageType !ByteString |
  {-| Combines both sending and aggregation for atomicity -}
  SendAndAggregateMessage !ByteString !H.Interpreter !(Error -> IO ()) |
  TransportErrorMessage !Text |
  ProtocolErrorMessage !Text

actor :: A.Actor C.Message -> IO (A.Actor Message)
actor sender =
  do
    interpreterRef <- newIORef mempty
    failerRef <- newIORef mempty
    handlersQueueRef <- newIORef mempty
    busyRef <- newIORef False
    let
      addHandler interpreter failer =
        do
          busy <- readIORef busyRef
          if busy
            then modifyIORef handlersQueueRef (B.snoc handler)
            else do
              writeIORef busyRef True
              writeIORef interpreterRef interpreter
              writeIORef failerRef failer
        where
          handler =
            (interpreter, failer)
      activateNextHandler =
        do
          handlersQueue <- readIORef handlersQueueRef
          case B.uncons handlersQueue of
            Just ((interpreter, failer), tail) ->
              do
                writeIORef busyRef True
                writeIORef interpreterRef interpreter
                writeIORef failerRef failer
                writeIORef handlersQueueRef tail
            Nothing ->
              do
                writeIORef busyRef False
                writeIORef interpreterRef mempty
                writeIORef failerRef mempty
      fail error =
        do
          failer <- readIORef failerRef
          failer error
      interpret messageType messageBytes =
        do
          H.Interpreter interpret <- readIORef interpreterRef
          interpret messageType messageBytes
      in
        A.graceful $ \case
          BackendMessageMessage messageType messagePayloadBytes ->
            do
              keep <- interpret messageType messageBytes
              unless keep activateNextHandler
          SendAndAggregateMessage messageBytes interpreter failer ->
            do
              A.tell sender (C.SendMessage messageBytes (failer . TransportError))
              addHandler interpreter failer
          TransportErrorMessage message ->
            fail (TransportError message)
          ProtocolErrorMessage message ->
            fail (ProtocolError message)
