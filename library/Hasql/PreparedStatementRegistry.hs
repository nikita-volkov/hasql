module Hasql.PreparedStatementRegistry
  ( PreparedStatementRegistry,
    new,
    update,
    reset,
    LocalKey (..),
  )
where

import Hasql.Prelude hiding (lookup, reset)
import Hasql.Structures.RegistryState (LocalKey (..))
import Hasql.Structures.RegistryState qualified as Map

-- | Registry data structure containing a pure RegistryState wrapped in IORef
data PreparedStatementRegistry
  = PreparedStatementRegistry !(IORef Map.RegistryState)

{-# INLINEABLE new #-}
new :: IO PreparedStatementRegistry
new =
  PreparedStatementRegistry <$> newIORef Map.empty

{-# INLINEABLE update #-}
update :: LocalKey -> (ByteString -> IO (Bool, a)) -> (ByteString -> IO a) -> PreparedStatementRegistry -> IO a
update localKey onNewRemoteKey onOldRemoteKey (PreparedStatementRegistry registryRef) = do
  registryState <- readIORef registryRef
  case Map.lookup localKey registryState of
    Just remoteKey -> onOldRemoteKey remoteKey
    Nothing -> do
      let (remoteKey, newState) = Map.insert localKey registryState
      (save, result) <- onNewRemoteKey remoteKey
      when save $ writeIORef registryRef newState
      return result

reset :: PreparedStatementRegistry -> IO ()
reset (PreparedStatementRegistry registryRef) = do
  writeIORef registryRef Map.empty
