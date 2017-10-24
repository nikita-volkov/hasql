module Hasql.Core.Loops.Receiver where

import Hasql.Prelude
import Hasql.Core.Model
import qualified Hasql.Core.Socket as A
import qualified Hasql.Core.Protocol.Peek as E
import qualified Buffer as C
import qualified Ptr.Peek as D


{-# INLINABLE loop #-}
loop :: A.Socket -> (Response -> IO ()) -> (Text -> IO ()) -> IO ()
loop socket sendResponse reportTransportError =
  {-# SCC "loop" #-} 
  do
    buffer <- C.new 16384
    withBuffer buffer
  where
    withBuffer buffer =
      load
      where
        receiveToBuffer failure success =
          C.push buffer 8192 $ \ptr -> do
            result <- A.receiveToPtr socket ptr 8192
            case result of
              Right amountReceived -> return (amountReceived, success)
              Left error -> return (0, failure error)
        peekFromBuffer :: D.Peek a -> (a -> IO ()) -> IO ()
        peekFromBuffer (D.Peek amount ptrIO) succeed =
          fix $ \recur ->
          join $ C.pull buffer amount (fmap succeed . {-# SCC "loop/peeking" #-} ptrIO) $ \_ ->
          receiveToBuffer reportTransportError recur
        load =
          peekFromBuffer E.response $ \bodyPeek ->
          peekFromBuffer bodyPeek $ \case
            Just (Just (Right !response)) -> sendResponse response >> load
            Just (Just (Left error)) -> $(todo "Handle message parsing error")
            Just Nothing -> load
            Nothing -> $(todo "Handle corrupt data")
