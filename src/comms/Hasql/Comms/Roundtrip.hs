module Hasql.Comms.Roundtrip
  ( Roundtrip,

    -- * Constructors
    prepare,
    queryPrepared,
    queryParams,
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

-- | Execute a script (multi-statement SQL).
-- It consumes all results from the execution,
-- which is necessary for scripts containing multiple statements.
script :: tag -> ByteString -> Roundtrip tag ()
script tag sql =
  Roundtrip
    (Send.query tag sql)
    (Recv.allResults tag ResultDecoder.ok)

-- | Error of a round trip, carrying the tag of the action that caused it.
--
-- A 'ClientError' means the send itself failed - either nothing reached the
-- server and nothing will until the connection is replaced, or the
-- connection is still usable but libpq refused the request outright (e.g.
-- more than 65535 parameters, or a command issued while another is in
-- progress). Both finish the connection the same way (see
-- 'Hasql.Engine.Errors.fromSendError'), so this carries no further
-- distinction between them.
data Error tag
  = ClientError
      tag
      Text
  | ServerError (Recv.Error tag)
  deriving stock (Show, Eq, Functor)

instance Comonad Error where
  {-# INLINE extract #-}
  extract = \case
    ClientError tag _ -> tag
    ServerError recvError -> extract recvError

  {-# INLINE duplicate #-}
  duplicate = \case
    clientError@(ClientError _ details) -> ClientError clientError details
    ServerError recvError -> ServerError (fmap ServerError (duplicate recvError))

-- | Run a round trip in pipeline mode, entering the mode and leaving it
-- again before returning.
--
-- The exit is attempted whether or not the receive succeeded, because a
-- failing receive still leaves the mode on and the connection unusable for
-- serial commands. It is viable on that path because 'Recv' does not
-- short-circuit: its 'Applicative' runs every step, so the results of every
-- dispatched command are consumed even when one of them decodes into an
-- error, and the queue libpq requires empty before it permits the exit is
-- empty by the time we ask.
--
-- A failed /send/ is the one case with no exit attempt, and deliberately so.
-- It strands commands that earlier callers in the same batch queued but
-- never flushed, and no amount of draining makes that connection's protocol
-- state something we can vouch for. Such a connection is finished by
-- 'Hasql.Connection.use' rather than repaired: both errors a send failure
-- can produce say so (see 'ClientError'), and pushing a Sync to salvage it
-- would flush and commit the very commands the caller's pipeline never got
-- to complete.
--
-- Of the two errors an unhappy path can carry, the exit failure wins. It is
-- both the rarer and the graver: a mode that cannot be left means the
-- connection serves nothing serial from then on, whereas the failure that got
-- us here is quite possibly an ordinary server error the caller is meant to
-- catch and carry on from. Reporting the latter would return a
-- 'Hasql.Engine.Errors.SessionUseError' - catchable, connection retained -
-- and hand it back still in pipeline mode, which is
-- <https://github.com/nikita-volkov/hasql/issues/326> over again.
toPipelineIO :: Roundtrip tag a -> tag -> Pq.Connection -> IO (Either (Error tag) a)
toPipelineIO (Roundtrip send recv) tag connection = runExceptT do
  ExceptT (runSend (Send.enterPipelineMode tag <> send <> Send.pipelineSync tag) connection)
  recvResult <- lift (first ServerError <$> Recv.toHandler (recv <* Recv.singleResult tag ResultDecoder.pipelineSync) connection)
  exitResult <- lift (runSend (Send.exitPipelineMode tag) connection)
  ExceptT (pure (exitResult *> recvResult))

-- | Unlike 'toPipelineIO', this never enters pipeline mode, so there is no
-- mode to leave: every call site sends a single operation and there is
-- nothing queued after it.
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
      let message = maybe "" decodeUtf8Lenient errorMessage
      pure (Left (ClientError tag message))
