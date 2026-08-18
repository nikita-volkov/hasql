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
  sendResult <- interpretSendResult connection sendResult
  case sendResult of
    Left err -> pure (Left err)
    Right () -> do
      recvResult <- first ServerError <$> restore (Recv.toHandler recv connection)
      exitResult <- do
        result <- Send.toHandler (Send.exitPipelineMode tag) connection
        interpretSendResult connection result
      pure (recvResult <* exitResult)
  where
    Roundtrip send recv = sendAndRecv <* pipelineSync tag

toSerialIO :: Roundtrip tag a -> Pq.Connection -> IO (Either (Error tag) a)
toSerialIO (Roundtrip send recv) connection = do
  sendResult <- Send.toHandler send connection
  sendResult <- interpretSendResult connection sendResult
  case sendResult of
    Left err -> pure (Left err)
    Right () -> do
      recvResult <- Recv.toHandler recv connection
      pure (first ServerError recvResult)

-- | Turn a failed send result into an 'Error', consulting the connection
-- for the diagnostic details (the send result itself no longer carries them).
interpretSendResult :: Pq.Connection -> Either tag () -> IO (Either (Error tag) ())
interpretSendResult connection = \case
  Right () -> pure (Right ())
  Left tag -> do
    errorMessage <- Pq.errorMessage connection
    status <- Pq.status connection
    pure (Left (ClientError tag (status == Pq.ConnectionBad) (maybe "" decodeUtf8Lenient errorMessage)))

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
