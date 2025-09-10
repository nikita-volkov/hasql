module Hasql.PreparedStatementRegistry
  ( PreparedStatementRegistry,
    new,
    update,
    reset,
    LocalKey (..),

    -- * Temporary hooks
    onPureState,
    readPureState,
    writePureState,
  )
where

import Hasql.Prelude hiding (lookup, reset)
import Hasql.Structures.StatementCache (LocalKey (..))
import Hasql.Structures.StatementCache qualified as Map

-- | Registry data structure containing a pure StatementCache wrapped in IORef
data PreparedStatementRegistry
  = PreparedStatementRegistry !(IORef Map.StatementCache)

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

onPureState :: PreparedStatementRegistry -> (Map.StatementCache -> (a, Map.StatementCache)) -> IO a
onPureState (PreparedStatementRegistry registryRef) f = do
  registryState <- readIORef registryRef
  let (result, newState) = f registryState
  writeIORef registryRef newState
  return result

readPureState :: PreparedStatementRegistry -> IO Map.StatementCache
readPureState (PreparedStatementRegistry registryRef) =
  readIORef registryRef

writePureState :: PreparedStatementRegistry -> Map.StatementCache -> IO ()
writePureState (PreparedStatementRegistry registryRef) =
  writeIORef registryRef
