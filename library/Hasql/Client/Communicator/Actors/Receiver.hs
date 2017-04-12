module Hasql.Client.Communicator.Actors.Receiver where

import Hasql.Prelude
import Hasql.Client.Model
import qualified Hasql.Protocol.Model as J
import qualified Hasql.Client.Communicator.Actors.Interpreter as E
import qualified Hasql.Client.Communicator.Receiver as K
import qualified Hasql.Client.Socket as F
import qualified Litsedey as B


data Message =
  ReceiveForeverMessage

actor :: F.Socket -> B.Actor E.Message -> IO (B.Actor Message)
actor socket interpreter =
  do
    receiver <- K.acquire socket
    B.disgraceful $ \case
      ReceiveForeverMessage ->
        loop ""
        where
          loop remainder =
            {-# SCC "actor/loop" #-} 
            do
              eitherMessage <- K.use receiver (K.getMessage E.BackendMessageMessage)
              case eitherMessage of
                Right message ->
                  do
                    B.tell interpreter message
                    loop remainder
                Left error ->
                  case error of
                    K.TransportError x ->
                      B.tell interpreter (E.TransportErrorMessage x)
                    K.PeekingError x ->
                      B.tell interpreter (E.ProtocolErrorMessage x)
