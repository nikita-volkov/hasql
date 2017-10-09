module Hasql.Loops.Receiver where

import Hasql.Prelude
import qualified Hasql.Socket as A
import qualified Data.ByteString as B


{-# INLINABLE loop #-}
loop :: A.Socket -> (ByteString -> IO ()) -> (Text -> IO ()) -> IO ()
loop socket sendResult reportError =
  forever $ do
    traceEventIO "START Receiver/receive"
    resultOfReceiving <- A.receive socket (shiftL 2 12)
    traceEventIO "STOP Receiver/receive"
    case resultOfReceiving of
      Right bytes ->
        if B.null bytes
          then reportError "Connection interrupted"
          else sendResult bytes
      Left msg ->
        reportError msg
