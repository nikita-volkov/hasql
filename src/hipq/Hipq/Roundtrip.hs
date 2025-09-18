module Hipq.Roundtrip
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

import Hipq.Recv qualified as Recv
import Hipq.ResultDecoder qualified as ResultDecoder
import Hipq.Send qualified as Send
import Platform.Prelude
import Pq qualified

data Roundtrip a
  = Roundtrip Send.Send (Recv.Recv a)
  deriving stock (Functor)

instance Applicative Roundtrip where
  {-# INLINE pure #-}
  pure x = Roundtrip mempty (pure x)
  {-# INLINE (<*>) #-}
  Roundtrip send1 recv1 <*> Roundtrip send2 recv2 =
    Roundtrip (send1 <> send2) (recv1 <*> recv2)

toPipelineIO :: Roundtrip a -> Pq.Connection -> IO (Either Error a)
toPipelineIO sendAndRecv connection = do
  sendResult <- Send.toHandler (Send.enterPipelineMode <> send) connection
  case sendResult of
    Left sendError -> pure (Left (ClientError sendError))
    Right () -> do
      recvResult <- Recv.toHandler recv connection
      exitResult <- Send.toHandler Send.exitPipelineMode connection
      pure (first RecvError recvResult <* first ClientError exitResult)
  where
    Roundtrip send recv = sendAndRecv <* pipelineSync

toSerialIO :: Roundtrip a -> Pq.Connection -> IO (Either Error a)
toSerialIO (Roundtrip send recv) connection = do
  sendResult <- Send.toHandler send connection
  case sendResult of
    Left sendError -> pure (Left (ClientError sendError))
    Right () -> do
      recvResult <- Recv.toHandler recv connection
      pure (first RecvError recvResult)

pipelineSync :: Roundtrip ()
pipelineSync =
  Roundtrip
    Send.pipelineSync
    (Recv.singleResult ResultDecoder.pipelineSync)

prepare :: ByteString -> ByteString -> [Pq.Oid] -> Roundtrip ()
prepare statementName sql oidList =
  Roundtrip
    (Send.prepare statementName sql (Just oidList))
    (Recv.singleResult ResultDecoder.ok)

queryPrepared ::
  -- | Prepared statement name.
  ByteString ->
  -- | Parameters.
  [Maybe (ByteString, Pq.Format)] ->
  -- | Result format.
  Pq.Format ->
  -- | Result decoder.
  ResultDecoder.ResultDecoder a ->
  Roundtrip a
queryPrepared statementName params resultFormat resultDecoder =
  Roundtrip
    (Send.queryPrepared statementName params resultFormat)
    (Recv.singleResult resultDecoder)

queryParams ::
  -- | SQL.
  ByteString ->
  -- | Parameters.
  [Maybe (Pq.Oid, ByteString, Pq.Format)] ->
  -- | Result format.
  Pq.Format ->
  -- | Result decoder.
  ResultDecoder.ResultDecoder a ->
  Roundtrip a
queryParams sql params resultFormat resultDecoder =
  Roundtrip
    (Send.queryParams sql params resultFormat)
    (Recv.singleResult resultDecoder)

query :: ByteString -> Roundtrip ()
query sql =
  Roundtrip
    (Send.query sql)
    (Recv.singleResult ResultDecoder.ok)

data Error
  = ClientError (Maybe ByteString)
  | RecvError Recv.Error
  deriving stock (Show, Eq)
