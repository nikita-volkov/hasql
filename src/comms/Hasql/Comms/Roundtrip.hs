module Hasql.Comms.Roundtrip
  ( Roundtrip,

    -- * Constructors
    prepare,
    queryPrepared,
    queryParams,
    query,
    script,

    -- * Errors
    Error (..),

    -- * Execution
    toPipelineIO,
    toSerialIO,
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
      -- | Whether @PQstatus@ reported the connection as bad right after the
      -- send failed.
      --
      -- 'True' means nothing reached the server and nothing will until the
      -- connection is replaced. 'False' means the connection is still usable
      -- and libpq refused the request itself - e.g. more than 65535
      -- parameters, or a command issued while another is in progress - so the
      -- same request will be refused the same way on any connection.
      --
      -- This is all the callers need in order to decide whether the failure is
      -- worth retrying.
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
--
-- Hence the shape below: the happy path ends with the plain exit, which by
-- then has nothing left to do but turn the mode off, and every failing path
-- - whichever step it failed at - goes through 'leavePipeline' instead.
toPipelineIO :: Roundtrip tag a -> tag -> Pq.Connection -> IO (Either (Error tag) a)
toPipelineIO (Roundtrip send recv) tag connection = do
  result <- runExceptT do
    ExceptT (runSend (Send.enterPipelineMode tag <> send <> Send.pipelineSync tag) connection)
    result <- ExceptT (first ServerError <$> Recv.toHandler (recv <* Recv.singleResult tag ResultDecoder.pipelineSync) connection)
    ExceptT (runSend (Send.exitPipelineMode tag) connection)
    pure result
  case result of
    Right result -> pure (Right result)
    Left err -> Left err <$ leavePipeline
  where

    -- Get the connection out of pipeline mode after something in the
    -- pipeline went wrong. This is more than turning a flag off: libpq
    -- exits the mode only once the command queue is empty and every
    -- dispatched command's results have been consumed, and the server
    -- withholds those results until it sees a Sync.
    --
    -- So we send one - which also flushes whatever of ours is still sitting
    -- in the send buffer - and consume results up to the PipelineSync it
    -- produces. That result is where libpq resumes normal result
    -- processing; everything the aborted pipeline skipped comes back as
    -- PipelineAborted before it.
    --
    -- The rounds repeat because the batch's own Sync can still be queued
    -- ahead of ours, when the receive above failed before consuming it, and
    -- because a round also ends at a plain command boundary. They stop as
    -- soon as the exit is accepted, and stop regardless once a round finds
    -- nothing left to consume - no amount of further draining improves on
    -- that.
    --
    -- The outcome is not reported: the failure that got us here describes
    -- the connection better than the repair could.
    leavePipeline = do
      _ <- Pq.pipelineSync connection
      exitAfterDraining
      where
        exitAfterDraining = do
          consumedAnything <- drainToSyncPoint
          exited <- Pq.exitPipelineMode connection
          when (not exited && consumedAnything) exitAfterDraining
          where
            -- Consume results up to and including the next sync point, reporting
            -- whether anything got consumed.
            drainToSyncPoint =
              let go consumedAnything =
                    Pq.getResult connection >>= \case
                      -- A command boundary, or an empty queue.
                      Nothing -> pure consumedAnything
                      Just result ->
                        Pq.resultStatus result >>= \case
                          -- Terminated by a Nothing like any other command's round
                          -- of results.
                          Pq.PipelineSync -> True <$ Pq.getResult connection
                          _ -> go True
               in go False

-- | Unlike 'toPipelineIO', this never enters pipeline mode, so a send
-- failure here never leaves dispatched-but-unacknowledged commands behind:
-- every call site sends a single operation and there is nothing queued
-- after it to strand. No restoration step is needed.
toSerialIO :: Roundtrip tag a -> Pq.Connection -> IO (Either (Error tag) a)
toSerialIO (Roundtrip send recv) connection = do
  sendResult <- runSend send connection
  case sendResult of
    Left err -> pure (Left err)
    Right () -> do
      recvResult <- Recv.toHandler recv connection
      pure (first ServerError recvResult)

-- | Execute a send action on a connection, turning a failed send into an 'Error',
-- which consults the connection for the diagnostic details
-- (the send result itself no longer carries them).
runSend :: Send.Send tag -> Pq.Connection -> IO (Either (Error tag) ())
runSend send connection = do
  sendResult <- Send.toHandler send connection
  case sendResult of
    Right () -> pure (Right ())
    Left tag -> do
      errorMessage <- Pq.errorMessage connection
      status <- Pq.status connection
      let connectionLost = status == Pq.ConnectionBad
          message = maybe "" decodeUtf8Lenient errorMessage
      pure (Left (ClientError tag connectionLost message))
