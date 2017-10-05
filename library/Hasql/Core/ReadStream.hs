module Hasql.Core.ReadStream where

import Hasql.Prelude
import qualified Hasql.Socket as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified PtrMagic.Decoder as C
import qualified PtrMagic.Encoding as D
import qualified PtrMagic.ByteString as E


newtype ReadStream output =
  ReadStream (ReaderT (IO ByteString) (StateT ByteString IO) output)
  deriving (Functor, Applicative, Monad, MonadIO)

run :: ReadStream output -> IO ByteString -> IO (output, ByteString)
run (ReadStream stack) fetchChunk =
  runStateT (runReaderT stack fetchChunk) ""

streamReader :: (IO ByteString -> ByteString -> IO (output, ByteString)) -> ReadStream output
streamReader fn =
  ReadStream (ReaderT (\fetchChunk -> StateT (\cached -> fn fetchChunk cached)))

fetchBytes :: Int -> ReadStream ByteString
fetchBytes amount =
  streamReader $ \fetchChunk cached ->
  if B.length cached < amount
    then fetchAccumulating fetchChunk (D.bytes cached)
    else return (B.splitAt amount cached)
  where
    fetchAccumulating fetchChunk encoding =
      do
        bytes <- fetchChunk
        if requiredAmount > B.length bytes
          then fetchAccumulating fetchChunk (encoding <> D.bytes bytes)
          else case B.splitAt requiredAmount bytes of
            (consumedBytes, remainingBytes) ->
              return (E.encoding (encoding <> D.bytes consumedBytes), remainingBytes)
      where
        requiredAmount =
          case encoding of
            D.Encoding length _ -> amount - length

decode :: C.Decoder decoded -> ReadStream decoded
decode (C.Decoder amount action) =
  do
    B.PS fp offset _ <- fetchBytes amount
    liftIO (withForeignPtr fp (\p -> action (plusPtr p offset)))

fetchMessage :: (Word8 -> ByteString -> message) -> ReadStream message
fetchMessage message =
  join (decode headerDecoder)
  where
    headerDecoder =
      readMessage <$> C.word8 <*> (subtract 4 . fromIntegral <$> C.beWord32)
    readMessage messageType payloadLength =
      message messageType <$> fetchBytes payloadLength
