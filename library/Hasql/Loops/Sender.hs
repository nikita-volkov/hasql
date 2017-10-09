module Hasql.Loops.Sender where

import Hasql.Prelude
import qualified Hasql.Socket as A
import qualified Data.ByteString as B


{-# INLINABLE loop #-}
loop :: A.Socket -> IO ByteString -> (Text -> IO ()) -> IO ()
loop socket getNextChunk reportError =
  fix $ \loop -> do
    bytes <- getNextChunk
    resultOfSending <- A.send socket bytes
    case resultOfSending of
      Right () -> loop
      Left msg -> reportError msg
