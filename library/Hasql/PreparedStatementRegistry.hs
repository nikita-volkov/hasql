module Hasql.PreparedStatementRegistry
  ( PreparedStatementRegistry,
    new,
    update,
    reset,
    LocalKey (..),
  )
where

import ByteString.StrictBuilder qualified as B
import Data.HashMap.Strict qualified as HM
import Hasql.LibPq14 qualified as Pq
import Hasql.Prelude hiding (lookup, reset)

-- | Registry data structure containing a pure HashMap and counter wrapped in IORef
data PreparedStatementRegistry
  = PreparedStatementRegistry !(IORef (HM.HashMap LocalKey ByteString, Word))

{-# INLINEABLE new #-}
new :: IO PreparedStatementRegistry
new =
  PreparedStatementRegistry <$> newIORef (HM.empty, 0)

{-# INLINEABLE update #-}
update :: LocalKey -> (ByteString -> IO (Bool, a)) -> (ByteString -> IO a) -> PreparedStatementRegistry -> IO a
update localKey onNewRemoteKey onOldRemoteKey (PreparedStatementRegistry registryRef) = do
  (hashMap, counter) <- readIORef registryRef
  case HM.lookup localKey hashMap of
    Just remoteKey -> onOldRemoteKey remoteKey
    Nothing -> do
      let remoteKey = B.builderBytes . B.asciiIntegral $ counter
      (save, result) <- onNewRemoteKey remoteKey
      when save $ do
        let newHashMap = HM.insert localKey remoteKey hashMap
            newCounter = succ counter
        writeIORef registryRef (newHashMap, newCounter)
      return result

reset :: PreparedStatementRegistry -> IO ()
reset (PreparedStatementRegistry registryRef) = do
  writeIORef registryRef (HM.empty, 0)

-- |
-- Local statement key.
data LocalKey
  = LocalKey !ByteString ![Pq.Oid]
  deriving (Show, Eq)

instance Hashable LocalKey where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt (LocalKey template _) =
    hashWithSalt salt template
