module Hasql.Loops.IncomingMessagesSlicer where

import Hasql.Prelude
import Hasql.Model
import qualified Hasql.ReadStream as C


{-# INLINABLE loop #-}
loop :: IO ByteString -> (Message -> IO ()) -> IO ()
loop getNextChunk sendMessage =
  C.run read getNextChunk $> ()
  where
    read =
      do
        message <- C.fetchMessage Message
        liftIO (sendMessage message)
        read
