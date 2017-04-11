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

newtype Session result =
  Session (ReaderT Socket (ExceptT Text IO) result)
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Text)

{-# INLINE trySocketIO #-}
trySocketIO :: IO a -> IO (Either Text a)
trySocketIO io =
  catchIOError (fmap Right io) (return . Left . socketExceptionText)
  where
    socketExceptionText e =
      (fromString . show) e

connectToHostAndPort :: ByteString -> Word16 -> IO (Either Text Socket)
connectToHostAndPort host port =
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

{-# INLINE use #-}
use :: Socket -> Session result -> IO (Either Text result)
use socket (Session (ReaderT def)) =
  runExceptT (def socket)

{-# INLINE io #-}
io :: (Socket -> IO (Either Text result)) -> Session result
io cont =
  Session (ReaderT (\socket -> ExceptT (cont socket)))

{-# INLINE socketIO #-}
socketIO :: (Socket -> IO result) -> Session result
socketIO cont =
  io (trySocketIO . cont)

{-# INLINE receive #-}
receive :: Int -> Session ByteString
receive amount =
  {-# SCC "receive" #-} 
  socketIO (\(Socket socket) -> C.recv socket amount)

{-# INLINE send #-}
send :: ByteString -> Session ()
send bytes =
  {-# SCC "send" #-} 
  socketIO (\(Socket socket) -> C.sendAll socket bytes)
