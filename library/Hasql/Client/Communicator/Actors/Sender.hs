module Hasql.Client.Communicator.Actors.Sender where

import Hasql.Prelude
import Hasql.Client.Model
import qualified Hasql.Client.Socket as F
import qualified Litsedey as A


data Message =
  SendMessage !ByteString !(Text -> IO ())

actor :: F.Socket -> IO (A.Actor Message)
actor socket =
  A.graceful $ {-# SCC "actor/interpret" #-} \case
    SendMessage encodedMessage handleError ->
      F.send socket encodedMessage >>= either handleError (const (return ()))
