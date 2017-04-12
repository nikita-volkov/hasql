module Hasql.Client.Socket where

import Hasql.Prelude
import Hasql.Protocol.Model
import qualified ByteString.StrictBuilder as F
import qualified Data.ByteString as G
import qualified Data.ByteString.Char8 as H
import qualified Network as A
import qualified Network.Socket as B
import qualified Network.Socket.ByteString as C


newtype Socket =
  Socket B.Socket

{-# INLINE trySocketIO #-}
trySocketIO :: IO a -> IO (Either Text a)
trySocketIO io =
  catchIOError (fmap Right io) (return . Left . socketExceptionText)
  where
    socketExceptionText e =
      (fromString . show) e

connectToHostAndPort :: ByteString -> Word16 -> IO (Either Text Socket)
connectToHostAndPort host port =
  do
    traceEventIO ("connectToHostAndPort")
    runExceptT $ do
      addrList <- getAddressInfo
      addr <- headFailing "Invalid host or port" addrList
      socket <- initSocket (B.addrFamily addr) (B.addrSocketType addr) (B.addrProtocol addr)
      connect socket (B.addrAddress addr)
      return (Socket socket)
  where
    io =
      ExceptT . trySocketIO
    getAddressInfo =
      io (B.getAddrInfo (Just hints) (Just hostString) (Just portString))
      where
        hints =
          B.defaultHints {
            B.addrFlags = [B.AI_V4MAPPED],
            B.addrSocketType = B.Stream
          }
        portString =
          show port
        hostString =
          H.unpack host
    headFailing message =
      \case
        x : _ ->
          return x
        _ ->
          throwE message
    initSocket family socketType protocolNumber =
      io (B.socket family socketType protocolNumber)
    connect socket socketAddress =
      io (B.connect socket socketAddress)

{-# INLINE close #-}
close :: Socket -> IO ()
close (Socket def) =
  B.close def

{-# INLINE receive #-}
receive :: Socket -> Int -> IO (Either Text ByteString)
receive (Socket socket) amount =
  {-# SCC "receive" #-} 
  trySocketIO (C.recv socket amount)

{-# INLINE receiveToPtr #-}
receiveToPtr :: Socket -> Ptr Word8 -> Int -> IO (Either Text Int)
receiveToPtr (Socket socket) ptr amount =
  {-# SCC "receiveToPtr" #-} 
  trySocketIO (B.recvBuf socket ptr amount)

{-# INLINE send #-}
send :: Socket -> ByteString -> IO (Either Text ())
send (Socket socket) bytes =
  {-# SCC "send" #-} 
  trySocketIO (C.sendAll socket bytes)
