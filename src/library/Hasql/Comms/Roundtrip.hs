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
import Hasql.Driver.Interface qualified as Interface
import Hasql.Platform.Prelude

data Roundtrip conn context a
  = Roundtrip (Send.Send conn context) (Recv.Recv conn context a)
  deriving stock (Functor)

instance Applicative (Roundtrip conn context) where
  {-# INLINE pure #-}
  pure x = Roundtrip mempty (pure x)
  {-# INLINE (<*>) #-}
  Roundtrip send1 recv1 <*> Roundtrip send2 recv2 =
    Roundtrip (send1 <> send2) (recv1 <*> recv2)

instance Bifunctor (Roundtrip conn) where
  {-# INLINE bimap #-}
  bimap f g (Roundtrip send recv) =
    Roundtrip
      (fmap f send)
      (bimap f g recv)

toPipelineIO :: Interface.Driver conn result -> Roundtrip conn context a -> context -> conn -> IO (Either (Error context) a)
toPipelineIO drv sendAndRecv context connection = mask \restore -> do
  sendResult <- Send.toHandler (Send.enterPipelineMode drv context <> send) connection
  case sendResult of
    Send.Error context details -> pure (Left (ClientError context details))
    Send.Ok -> do
      recvResult <- first ServerError <$> restore (Recv.toHandler recv connection)
      exitResult <- do
        result <- Send.toHandler (Send.exitPipelineMode drv context) connection
        case result of
          Send.Error context details -> pure (Left (ClientError context details))
          Send.Ok -> pure (Right ())
      pure (recvResult <* exitResult)
  where
    Roundtrip send recv = sendAndRecv <* pipelineSync drv context

toSerialIO :: Roundtrip conn context a -> conn -> IO (Either (Error context) a)
toSerialIO (Roundtrip send recv) connection = do
  sendResult <- Send.toHandler send connection
  case sendResult of
    Send.Error context details -> pure (Left (ClientError context details))
    Send.Ok -> do
      recvResult <- Recv.toHandler recv connection
      pure (first ServerError recvResult)

pipelineSync :: Interface.Driver conn result -> context -> Roundtrip conn context ()
pipelineSync drv context =
  Roundtrip
    (Send.pipelineSync drv context)
    (Recv.singleResult drv context ResultDecoder.pipelineSync)

prepare :: Interface.Driver conn result -> context -> ByteString -> ByteString -> [Word32] -> Roundtrip conn context ()
prepare drv context statementName sql oidList =
  Roundtrip
    (Send.prepare drv context statementName sql oidList)
    (Recv.singleResult drv context ResultDecoder.ok)

queryPrepared ::
  Interface.Driver conn result ->
  context ->
  -- | Prepared statement name.
  ByteString ->
  -- | Parameters.
  [Maybe (ByteString, Bool)] ->
  -- | Result decoder.
  ResultDecoder.ResultDecoder a ->
  Roundtrip conn context a
queryPrepared drv context statementName params resultDecoder =
  Roundtrip
    (Send.queryPrepared drv context statementName params)
    (Recv.singleResult drv context resultDecoder)

queryParams ::
  Interface.Driver conn result ->
  context ->
  -- | SQL.
  ByteString ->
  -- | Parameters.
  [Maybe (Word32, ByteString, Bool)] ->
  -- | Result decoder.
  ResultDecoder.ResultDecoder a ->
  Roundtrip conn context a
queryParams drv context sql params resultDecoder =
  Roundtrip
    (Send.queryParams drv context sql params)
    (Recv.singleResult drv context resultDecoder)

query :: Interface.Driver conn result -> context -> ByteString -> Roundtrip conn context ()
query drv context sql =
  Roundtrip
    (Send.query drv context sql)
    (Recv.singleResult drv context ResultDecoder.ok)

-- | Execute a script (multi-statement SQL).
-- Unlike 'query', this consumes all results from the execution,
-- which is necessary for scripts containing multiple statements.
script :: Interface.Driver conn result -> context -> ByteString -> Roundtrip conn context ()
script drv context sql =
  Roundtrip
    (Send.query drv context sql)
    (Recv.allResults drv context ResultDecoder.ok)

data Error context
  = ClientError context (Maybe ByteString)
  | ServerError (Recv.Error context)
  deriving stock (Show, Eq)
