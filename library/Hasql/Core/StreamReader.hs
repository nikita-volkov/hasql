module Hasql.Core.StreamReader where

import Hasql.Prelude
import qualified Hasql.Socket as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified PtrMagic.Decoder as C
import qualified PtrMagic.Encoding as D
import qualified PtrMagic.ByteString as E


newtype StreamReader output =
  StreamReader (ReaderT (IO ByteString) (StateT ByteString IO) output)
  deriving (Functor, Applicative, Monad, MonadIO)

run :: StreamReader output -> IO ByteString -> IO (output, ByteString)
run (StreamReader stack) fetchChunk =
  runStateT (runReaderT stack fetchChunk) ""

streamReader :: (IO ByteString -> ByteString -> IO (output, ByteString)) -> StreamReader output
streamReader fn =
  StreamReader (ReaderT (\fetchChunk -> StateT (\cached -> fn fetchChunk cached)))

fetchBytes :: Int -> StreamReader ByteString
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

decode :: C.Decoder decoded -> StreamReader decoded
decode (C.Decoder amount action) =
  do
    B.PS fp offset _ <- fetchBytes amount
    liftIO (withForeignPtr fp (\p -> action (plusPtr p offset)))

fetchMessage :: (Word8 -> ByteString -> message) -> StreamReader message
fetchMessage message =
  join (decode headerDecoder)
  where
    headerDecoder =
      readMessage <$> C.word8 <*> (subtract 4 . fromIntegral <$> C.beWord32)
    readMessage messageType payloadLength =
      message messageType <$> fetchBytes payloadLength
