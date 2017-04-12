module Hasql.Client.Communicator.Receiver where

import Hasql.Prelude hiding (peek)
import qualified Hasql.Protocol.Model as J
import qualified Hasql.Client.Socket as F
import qualified GHC.IO.Buffer as E
import qualified Hasql.Ptr.Peek as C


data Error =
  TransportError Text |
  ParsingError Text


{-|
A specialized buffered socket reader.
-}
data Receiver =
  Receiver F.Socket (E.Buffer Word8)

acquire :: F.Socket -> IO Receiver
acquire socket =
  Receiver socket <$> acquireBuffer
  where
    acquireBuffer =
      E.newByteBuffer (shiftL 1 14) E.WriteBuffer


newtype Do result =
  Do (ReaderT Receiver (ExceptT Error IO) result)
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Error)

io :: (Receiver -> IO (Either Error result)) -> Do result
io def =
  Do (ReaderT (\receiver -> ExceptT (def receiver)))

{-|
Populate the buffer by fetching the data from socket.
-}
fetchFromSocket :: Int -> Do ()
fetchFromSocket amount =
  io $ \(Receiver socket buffer) -> do
    traceEventIO ("fetchFromSocket " <> show amount)
    $(todo "")
  where
    actualAmount =
      max amount (shiftL 1 13)

{-|
Ensure that there is a certain amount of bytes available in the buffer,
blocking until that.

Initiates the socket fetching if need be.
-}
demandAmount :: Int -> Do ()
demandAmount amount =
  $(todo "")

peek :: C.Peek peeked -> Do peeked
peek peek =
  case C.run peek of
    (amount, ptrIO) ->
      do
        demandAmount amount
        io $ \(Receiver _ buffer) -> fmap Right $ E.withBuffer buffer ptrIO

getMessageHeader :: (J.MessageType -> Int -> result) -> Do result
getMessageHeader cont =
  peek peeker
  where
    peeker =
      cont <$> (J.MessageType <$> C.word8) <*> (fromIntegral <$> C.beWord32)

getMessageBytes :: Int -> Do ByteString
getMessageBytes amount =
  peek (C.bytes amount)

getMessage :: (J.MessageType -> ByteString -> result) -> Do result
getMessage cont =
  join $ getMessageHeader $ \messageType messageLength ->
  do
    bytes <- getMessageBytes messageLength
    return (cont messageType bytes)
