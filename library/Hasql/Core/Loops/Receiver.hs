module Hasql.Core.Loops.Receiver where

import Hasql.Prelude
import qualified Hasql.Socket as A
import qualified Data.ByteString as B


{-# INLINABLE loop #-}
loop :: A.Socket -> (ByteString -> IO ()) -> (Text -> IO ()) -> IO ()
loop socket sendResult reportError =
  fix $ \loop -> do
    resultOfReceiving <- A.receive socket (shiftL 2 12)
    case resultOfReceiving of
      Right bytes ->
        if B.null bytes
          then reportError "Connection interrupted"
          else sendResult bytes >> loop
      Left msg ->
        reportError msg
