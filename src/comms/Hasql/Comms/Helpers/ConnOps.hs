-- |
-- The pipeline mode of a connection.
--
-- While the mode is on libpq refuses every serial command and the server
-- withholds the results of the dispatched ones until it receives a Sync, so
-- turning the mode off again is a protocol obligation of whoever turned it
-- on. This module provides the operation that discharges it, shared by
-- "Hasql.Comms.Roundtrip", which owns the mode for the span of one pipeline,
-- and "Hasql.Comms.Session", which repairs connections that got out of that
-- span dirty.
module Hasql.Comms.Helpers.ConnOps
  ( leave,
    drainProgressively,
  )
where

import Hasql.Platform.Prelude
import Pqi qualified as Pq

-- | Leave the pipeline mode of a connection, draining all the results that
-- the commands dispatched in it have produced or are still producing.
--
-- A connection in pipeline mode cannot serve serial commands: libpq rejects
-- them while the mode is on, and the server withholds the results of the
-- dispatched commands until it receives a Sync. Hence before turning the
-- mode off we send a Sync and a Flush and drain everything that comes back.
--
-- Draining is not a single pass: in pipeline mode @PQgetResult@ terminates
-- the round of results of every command with a 'Nothing', so a drain loop
-- stops at each command boundary. 'Pq.exitPipelineMode' only succeeds once
-- the command queue is empty, so draining and exit attempts alternate for as
-- long as draining keeps making progress.
--
-- Idempotent: a no-op when the connection is not in pipeline mode, costing
-- one local @PQpipelineStatus@ call and no network traffic.
leave :: Pq.Connection -> IO (Either Text ())
leave connection = do
  pipelineStatus <- Pq.pipelineStatus connection
  if pipelineStatus == Pq.PipelineOff
    then pure (Right ())
    else do
      _ <- Pq.pipelineSync connection
      void (drainProgressively connection)
      -- Ensure any queued commands the Sync above didn't flush on its own
      -- reach the server before we start waiting on results for them.
      _ <- Pq.sendFlushRequest connection
      void (drainProgressively connection)
      exitWithDraining
  where
    exitWithDraining =
      Pq.exitPipelineMode connection >>= \case
        True -> pure (Right ())
        False ->
          drainProgressively connection >>= \case
            True -> exitWithDraining
            False -> do
              errorMessage <- Pq.errorMessage connection
              pure (Left (maybe "" decodeUtf8Lenient errorMessage))

-- | Consume the results of the currently dispatched commands, reporting
-- whether anything got consumed.
--
-- In pipeline mode this drains up to the next command boundary, since every
-- command's round of results is terminated by a 'Nothing'.
drainProgressively :: Pq.Connection -> IO Bool
drainProgressively connection =
  let go hasConsumedResult =
        Pq.getResult connection >>= \case
          Just _ -> go True
          Nothing -> pure hasConsumedResult
   in go False
