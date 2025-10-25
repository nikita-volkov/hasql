module Comms.Roundtrip
  ( Roundtrip,
    toPipelineIO,
    toSerialIO,

    -- * Constructors
    prepare,
    queryPrepared,
    queryParams,
    query,

    -- * Errors
    Error (..),
  )
where

import Comms.Recv qualified as Recv
import Comms.ResultDecoder qualified as ResultDecoder
import Comms.Send qualified as Send
import Platform.Prelude
import Pq qualified

data Roundtrip context a
  = Roundtrip (Send.Send context) (Recv.Recv context a)
  deriving stock (Functor)

instance Applicative (Roundtrip context) where
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

toPipelineIO :: Roundtrip context a -> context -> Pq.Connection -> IO (Either (Error context) a)
toPipelineIO sendAndRecv context connection = mask \restore -> do
  sendResult <- Send.toHandler (Send.enterPipelineMode context <> send) connection
  case sendResult of
    Send.Error context details -> pure (Left (ClientError context details))
    Send.Ok -> do
      recvResult <- first ServerError <$> restore (Recv.toHandler recv connection)
      exitResult <- do
        result <- Send.toHandler (Send.exitPipelineMode context) connection
        case result of
          Send.Error context details -> pure (Left (ClientError context details))
          Send.Ok -> pure (Right ())
      pure (recvResult <* exitResult)
  where
    Roundtrip send recv = sendAndRecv <* pipelineSync context

toSerialIO :: Roundtrip context a -> Pq.Connection -> IO (Either (Error context) a)
toSerialIO (Roundtrip send recv) connection = do
  sendResult <- Send.toHandler send connection
  case sendResult of
    Send.Error context details -> pure (Left (ClientError context details))
    Send.Ok -> do
      recvResult <- Recv.toHandler recv connection
      pure (first ServerError recvResult)

pipelineSync :: context -> Roundtrip context ()
pipelineSync context =
  Roundtrip
    (Send.pipelineSync context)
    (Recv.singleResult context ResultDecoder.pipelineSync)

prepare :: context -> ByteString -> ByteString -> [Pq.Oid] -> Roundtrip context ()
prepare context statementName sql oidList =
  Roundtrip
    (Send.prepare context statementName sql (Just oidList))
    (Recv.singleResult context ResultDecoder.ok)

queryPrepared ::
  context ->
  -- | Prepared statement name.
  ByteString ->
  -- | Parameters.
  [Maybe (ByteString, Pq.Format)] ->
  -- | Result format.
  Pq.Format ->
  -- | Result decoder.
  ResultDecoder.ResultDecoder a ->
  Roundtrip context a
queryPrepared context statementName params resultFormat resultDecoder =
  Roundtrip
    (Send.queryPrepared context statementName params resultFormat)
    (Recv.singleResult context resultDecoder)

queryParams ::
  context ->
  -- | SQL.
  ByteString ->
  -- | Parameters.
  [Maybe (Pq.Oid, ByteString, Pq.Format)] ->
  -- | Result format.
  Pq.Format ->
  -- | Result decoder.
  ResultDecoder.ResultDecoder a ->
  Roundtrip context a
queryParams context sql params resultFormat resultDecoder =
  Roundtrip
    (Send.queryParams context sql params resultFormat)
    (Recv.singleResult context resultDecoder)

query :: context -> ByteString -> Roundtrip context ()
query context sql =
  Roundtrip
    (Send.query context sql)
    (Recv.singleResult context ResultDecoder.ok)

data Error context
  = ClientError context (Maybe ByteString)
  | ServerError (Recv.Error context)
  deriving stock (Show, Eq)
