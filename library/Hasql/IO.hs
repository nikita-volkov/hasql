-- |
-- An API of low-level IO operations.
module Hasql.IO where

import Hasql.Commands qualified as Commands
import Hasql.LibPq14 qualified as Pq
import Hasql.Prelude

{-# INLINE acquireConnection #-}
acquireConnection :: ByteString -> IO Pq.Connection
acquireConnection =
  Pq.connectdb

{-# INLINE releaseConnection #-}
releaseConnection :: Pq.Connection -> IO ()
releaseConnection connection =
  Pq.finish connection

{-# INLINE checkConnectionStatus #-}
checkConnectionStatus :: Pq.Connection -> IO (Maybe (Maybe ByteString))
checkConnectionStatus c = do
  s <- Pq.status c
  case s of
    Pq.ConnectionOk -> return Nothing
    _ -> fmap Just (Pq.errorMessage c)

{-# INLINE checkServerVersion #-}
checkServerVersion :: Pq.Connection -> IO (Maybe Int)
checkServerVersion c =
  fmap (mfilter (< 80200) . Just) (Pq.serverVersion c)

{-# INLINE getIntegerDatetimes #-}
getIntegerDatetimes :: Pq.Connection -> IO Bool
getIntegerDatetimes c =
  fmap decodeValue $ Pq.parameterStatus c "integer_datetimes"
  where
    decodeValue =
      \case
        Just "on" -> True
        _ -> False

{-# INLINE initConnection #-}
initConnection :: Pq.Connection -> IO ()
initConnection c =
  void $ Pq.exec c (Commands.asBytes (Commands.setEncodersToUTF8 <> Commands.setMinClientMessagesToWarning))
