module Hasql.Comms.Roundtrip
  ( Roundtrip,
    toPipelineIO,
    toSerialIO,
    leavePipelineMode,

    -- * Constructors
    prepare,
    queryPrepared,
    queryParams,
    query,
    script,

    -- * Errors
    Error (..),
  )
where

import Hasql.Comms.Recv qualified as Recv
import Hasql.Comms.ResultDecoder qualified as ResultDecoder
import Hasql.Comms.Send qualified as Send
import Hasql.Platform.Prelude
import Pqi qualified as Pq

-- | A send action paired with the matching receive action, forming a single protocol round trip.
--
-- The @tag@ type parameter is a value attached at construction,
-- which the error carries if the round trip fails.
data Roundtrip tag a
  = Roundtrip (Send.Send tag) (Recv.Recv tag a)
  deriving stock (Functor)

instance Applicative (Roundtrip tag) where
  {-# INLINE pure #-}
  pure x = Roundtrip mempty (pure x)
  {-# INLINE (<*>) #-}
  Roundtrip send1 recv1 <*> Roundtrip send2 recv2 =
    Roundtrip (send1 <> send2) (recv1 <*> recv2)

instance Bifunctor Roundtrip where
  {-# INLINE bimap #-}
  bimap f g (Roundtrip send recv) =
    Roundtrip
      (fmap f send)
      (bimap f g recv)

toPipelineIO :: Roundtrip tag a -> tag -> Pq.Connection -> IO (Either (Error tag) a)
toPipelineIO sendAndRecv tag connection = mask \restore -> do
  sendResult <- Send.toHandler (Send.enterPipelineMode tag <> send) connection
  case sendResult of
    Send.Error tag connectionLost rawDetails -> do
      -- The commands preceding the failed one have already been dispatched,
      -- and the connection is still in pipeline mode. Returning it in this
      -- state would leave it unusable for every subsequent operation, with
      -- each failure mislabelled as a retryable connection error. So cancel
      -- whatever of those commands is still executing (unless the
      -- connection is already lost, in which case there is nothing to
      -- cancel) and leave the mode, syncing and draining the results it
      -- withheld.
      when (not connectionLost) do
        restore do
          Pq.getCancel connection >>= \case
            Nothing -> pure ()
            Just cancel -> void (Pq.cancel cancel)
      leaveResult <- restore (leavePipelineMode connection)
      details <- case leaveResult of
        Right () -> pure (maybe "" decodeUtf8Lenient rawDetails)
        Left leaveDetails -> do
          -- Restoration itself failed: the connection's protocol state is
          -- indeterminate, so it must not be handed back to the pool as
          -- reusable. Finish it right here, at the point where the failure
          -- is discovered, rather than reporting a distinct error and
          -- relying on 'Hasql.Connection.use' to react to it.
          restore (Pq.finish connection)
          pure
            ( mconcat
                [ maybe "" ((<> "\n") . decodeUtf8Lenient) rawDetails,
                  "Failed to restore the connection after a send failure",
                  if leaveDetails == "" then "" else ": " <> leaveDetails
                ]
            )
      pure (Left (ClientError tag connectionLost details))
    Send.Ok -> do
      recvResult <- first ServerError <$> restore (Recv.toHandler recv connection)
      exitResult <- do
        result <- Send.toHandler (Send.exitPipelineMode tag) connection
        case result of
          Send.Error tag connectionLost details -> pure (Left (ClientError tag connectionLost (maybe "" decodeUtf8Lenient details)))
          Send.Ok -> pure (Right ())
      pure (recvResult <* exitResult)
  where
    Roundtrip send recv = sendAndRecv <* pipelineSync tag

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
-- Idempotent: a no-op when the connection is not in pipeline mode.
leavePipelineMode :: Pq.Connection -> IO (Either Text ())
leavePipelineMode connection = do
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

    -- Consume the results of the currently dispatched commands, reporting
    -- whether anything got consumed.
    --
    -- In pipeline mode this drains up to the next command boundary, since
    -- every command's round of results is terminated by a 'Nothing'.
    drainProgressively :: Pq.Connection -> IO Bool
    drainProgressively connection =
      let go hasConsumedResult =
            Pq.getResult connection >>= \case
              Just _ -> go True
              Nothing -> pure hasConsumedResult
       in go False

-- | Unlike 'toPipelineIO', this never enters pipeline mode, so a send
-- failure here never leaves dispatched-but-unacknowledged commands behind:
-- every call site sends a single operation and there is nothing queued
-- after it to strand. No restoration step is needed.
toSerialIO :: Roundtrip tag a -> Pq.Connection -> IO (Either (Error tag) a)
toSerialIO (Roundtrip send recv) connection = do
  sendResult <- Send.toHandler send connection
  case sendResult of
    Send.Error tag connectionLost details -> pure (Left (ClientError tag connectionLost (maybe "" decodeUtf8Lenient details)))
    Send.Ok -> do
      recvResult <- Recv.toHandler recv connection
      pure (first ServerError recvResult)

pipelineSync :: tag -> Roundtrip tag ()
pipelineSync tag =
  Roundtrip
    (Send.pipelineSync tag)
    (Recv.singleResult tag ResultDecoder.pipelineSync)

prepare :: tag -> ByteString -> ByteString -> [Word32] -> Roundtrip tag ()
prepare tag statementName sql oidList =
  Roundtrip
    (Send.prepare tag statementName sql (Just oidList))
    (Recv.singleResult tag ResultDecoder.ok)

queryPrepared ::
  tag ->
  -- | Prepared statement name.
  ByteString ->
  -- | Parameters.
  [Maybe (ByteString, Pq.Format)] ->
  -- | Result format.
  Pq.Format ->
  -- | Result decoder.
  ResultDecoder.ResultDecoder a ->
  Roundtrip tag a
queryPrepared tag statementName params resultFormat resultDecoder =
  Roundtrip
    (Send.queryPrepared tag statementName params resultFormat)
    (Recv.singleResult tag resultDecoder)

queryParams ::
  tag ->
  -- | SQL.
  ByteString ->
  -- | Parameters.
  [Maybe (Word32, ByteString, Pq.Format)] ->
  -- | Result format.
  Pq.Format ->
  -- | Result decoder.
  ResultDecoder.ResultDecoder a ->
  Roundtrip tag a
queryParams tag sql params resultFormat resultDecoder =
  Roundtrip
    (Send.queryParams tag sql params resultFormat)
    (Recv.singleResult tag resultDecoder)

query :: tag -> ByteString -> Roundtrip tag ()
query tag sql =
  Roundtrip
    (Send.query tag sql)
    (Recv.singleResult tag ResultDecoder.ok)

-- | Execute a script (multi-statement SQL).
-- Unlike 'query', this consumes all results from the execution,
-- which is necessary for scripts containing multiple statements.
script :: tag -> ByteString -> Roundtrip tag ()
script tag sql =
  Roundtrip
    (Send.query tag sql)
    (Recv.allResults tag ResultDecoder.ok)

-- | Error of a round trip, carrying the tag of the action that caused it.
data Error tag
  = ClientError
      tag
      -- | Whether the connection was reported as lost at the moment the send
      -- failed. See 'Send.Result'.
      Bool
      Text
  | ServerError (Recv.Error tag)
  deriving stock (Show, Eq, Functor)

instance Comonad Error where
  {-# INLINE extract #-}
  extract = \case
    ClientError tag _ _ -> tag
    ServerError recvError -> extract recvError

  {-# INLINE duplicate #-}
  duplicate = \case
    clientError@(ClientError _ connectionLost details) -> ClientError clientError connectionLost details
    ServerError recvError -> ServerError (fmap ServerError (duplicate recvError))
