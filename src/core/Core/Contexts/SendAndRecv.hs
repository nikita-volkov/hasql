module Core.Contexts.SendAndRecv
  ( SendAndRecv,
    toPipelineIO,

    -- * Constructors
    prepare,
    queryPrepared,
    queryParams,
  )
where

import Core.Contexts.Recv qualified as Recv
import Core.Contexts.ResultConsumer qualified as ResultConsumer
import Core.Contexts.Send qualified as Send
import Core.Errors
import Core.Structures.ConnectionState qualified as ConnectionState
import Core.Structures.StatementCache qualified as StatementCache
import Platform.Prelude
import Pq qualified

data SendAndRecv a
  = SendAndRecv Send.Send (Recv.Recv a)
  deriving stock (Functor)

instance Applicative SendAndRecv where
  {-# INLINE pure #-}
  pure x = SendAndRecv mempty (pure x)
  {-# INLINE (<*>) #-}
  SendAndRecv send1 recv1 <*> SendAndRecv send2 recv2 =
    SendAndRecv (send1 <> send2) (recv1 <*> recv2)

toPipelineIO :: SendAndRecv a -> Pq.Connection -> IO (Either CommandError a)
toPipelineIO sendAndRecv connection = do
  sendResult <- Send.toHandler send connection
  case sendResult of
    Left sendError -> pure (Left (ClientError sendError))
    Right () -> do
      recvResult <- Recv.toHandler recv connection
      exitResult <- Send.toHandler Send.exitPipelineMode connection
      pure (first recvToResultError recvResult <* first ClientError exitResult)
  where
    SendAndRecv send recv = sendAndRecv <* pipelineSync

    recvToResultError :: Recv.RecvError -> CommandError
    recvToResultError =
      error "TODO: recvToResultError"

pipelineSync :: SendAndRecv ()
pipelineSync =
  SendAndRecv
    Send.pipelineSync
    (Recv.singleResult ResultConsumer.pipelineSync)

prepare :: ByteString -> ByteString -> [Pq.Oid] -> SendAndRecv ()
prepare statementName sql oidList =
  SendAndRecv
    (Send.prepare statementName sql (Just oidList))
    (Recv.singleResult ResultConsumer.ok)

queryPrepared ::
  (Recv.HandlesResult f) =>
  -- | Prepared statement name.
  ByteString ->
  -- | Parameters.
  [Maybe (ByteString, Pq.Format)] ->
  -- | Result format.
  Pq.Format ->
  -- | Result handler.
  f a ->
  SendAndRecv a
queryPrepared statementName params resultFormat resultHandler =
  SendAndRecv
    (Send.queryPrepared statementName params resultFormat)
    (Recv.singleResult resultHandler)

queryParams ::
  (Recv.HandlesResult f) =>
  -- | SQL.
  ByteString ->
  -- | Parameters.
  [Maybe (Pq.Oid, ByteString, Pq.Format)] ->
  -- | Result format.
  Pq.Format ->
  -- | Result handler.
  f a ->
  SendAndRecv a
queryParams sql params resultFormat resultHandler =
  SendAndRecv
    (Send.queryParams sql params resultFormat)
    (Recv.singleResult resultHandler)
