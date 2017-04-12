module Hasql.Client.Communicator.Actors.Receiver where

import Hasql.Prelude
import Hasql.Client.Model
import qualified BinaryParser as A
import qualified Hasql.Protocol.Model as J
import qualified Hasql.Protocol.Decoding as D
import qualified Hasql.Client.Communicator.Actors.Interpreter as E
import qualified Hasql.Client.Socket as F
import qualified Litsedey as B
import qualified Hasql.Protocol.Scanner as G
import qualified Scanner as H


data Message =
  ReceiveForeverMessage

actor :: F.Socket -> B.Actor E.Message -> IO (B.Actor Message)
actor socket interpreter =
  B.disgraceful $ \case
    ReceiveForeverMessage ->
      loop ""
      where
        loop remainder =
          {-# SCC "actor/loop" #-} 
          do
            either <-
              {-# SCC "actor/loop/scan" #-} 
              let
                resupply =
                  ExceptT (F.receive socket 8192)
                scanner =
                  G.messageTypeAndPayload (,)
                in runExceptT (H.scanWith resupply scanner remainder)
            case either of
              Right scannerResult ->
                case scannerResult of
                  H.Done remainder (messageType, messagePayload) ->
                    do
                      B.tell interpreter (E.BackendMessageMessage messageType messagePayload)
                      loop remainder
                  H.Fail remainder scannerError ->
                    B.tell interpreter (E.ProtocolErrorMessage (fromString scannerError))
              Left transportError ->
                B.tell interpreter (E.TransportErrorMessage transportError)
