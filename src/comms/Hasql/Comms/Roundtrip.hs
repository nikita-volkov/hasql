module Hasql.Comms.Roundtrip
  ( Roundtrip,
    toPipelineIO,
    toSerialIO,

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

import Hasql.Comms.PipelineMode qualified as PipelineMode
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

-- | Run a round trip in pipeline mode, entering the mode and leaving it
-- again before returning.
--
-- Pipeline mode is scoped to this call, so leaving it is this function's
-- obligation on every path, not just the successful one. Two of them are
-- easy to miss: a send failure can strand commands that preceding callers
-- in the same batch already dispatched, and the ordinary exit attempt can
-- itself fail. Both used to return with the mode still on, leaving the
-- caller a connection that libpq refuses serial commands on and that hands
-- the next pipeline the stale results of this one. Repairing it after the
-- session returned - as 'Hasql.Connection.use' did - is too late: a session
-- is a 'MonadError' and can catch a pipeline failure and carry on, or catch
-- it and succeed, in which case the repair never runs at all.
toPipelineIO :: Roundtrip tag a -> tag -> Pq.Connection -> IO (Either (Error tag) a)
toPipelineIO sendAndRecv tag connection = do
  result <- attempt
  -- Idempotent, so on the common path where the exit above already
  -- succeeded this costs one local libpq call and no network traffic.
  leaveResult <- PipelineMode.leave connection
  pure case leaveResult of
    Right () -> result
    Left details -> case result of
      -- The failure that got us here explains the state better than the
      -- failed repair does, so it is the one reported.
      Left err -> Left err
      -- Nothing went wrong up to here, yet the mode would not come off:
      -- the connection's protocol state is indeterminate, which is a lost
      -- connection as far as callers are concerned.
      Right _ -> Left (ClientError tag True details)
  where
    attempt = do
      sendResult <- Send.toHandler (Send.enterPipelineMode tag <> send) connection
      case sendResult of
        Send.Error tag connectionLost details ->
          pure (Left (ClientError tag connectionLost (maybe "" decodeUtf8Lenient details)))
        Send.Ok -> do
          recvResult <- first ServerError <$> Recv.toHandler recv connection
          exitResult <- do
            result <- Send.toHandler (Send.exitPipelineMode tag) connection
            case result of
              Send.Error tag connectionLost details -> pure (Left (ClientError tag connectionLost (maybe "" decodeUtf8Lenient details)))
              Send.Ok -> pure (Right ())
          pure (recvResult <* exitResult)

    Roundtrip send recv = sendAndRecv <* pipelineSync tag

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
