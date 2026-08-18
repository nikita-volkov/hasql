module Hasql.Comms.Send
  ( Send,

    -- * Execution
    toHandler,

    -- * Constructors
    prepare,
    query,
    queryPrepared,
    queryParams,
    pipelineSync,
    enterPipelineMode,
    exitPipelineMode,
  )
where

import Hasql.Platform.Prelude
import Pqi qualified as Pq

-- | An action that sends commands to a connection without receiving the results.
--
-- The @err@ type parameter is a value attached to the commands,
-- which the error carries if sending fails.
newtype Send err
  = Send (Pq.Connection -> IO (Either err ()))

instance Functor Send where
  {-# INLINE fmap #-}
  fmap f (Send send) = Send \connection -> first f <$> send connection

instance Semigroup (Send err) where
  {-# INLINE (<>) #-}
  Send send1 <> Send send2 = Send \cs -> do
    result <- send1 cs
    case result of
      Left err -> pure (Left err)
      Right () -> send2 cs

instance Monoid (Send err) where
  {-# INLINE mempty #-}
  mempty = Send \_ -> pure (Right ())

toHandler :: Send err -> Pq.Connection -> IO (Either err ())
toHandler (Send send) = send

liftPqSend :: err -> (Pq.Connection -> IO Bool) -> Send err
liftPqSend err pqSend = Send \connection -> do
  success <- pqSend connection
  pure (if success then Right () else Left err)

prepare :: err -> ByteString -> ByteString -> Maybe [Word32] -> Send err
prepare err statementName sql oidList =
  liftPqSend err \connection -> Pq.sendPrepare connection statementName sql oidList

query :: err -> ByteString -> Send err
query err sql =
  liftPqSend err \connection -> Pq.sendQuery connection sql

queryPrepared :: err -> ByteString -> [Maybe (ByteString, Pq.Format)] -> Pq.Format -> Send err
queryPrepared err statementName params resultFormat =
  liftPqSend err \connection -> Pq.sendQueryPrepared connection statementName params resultFormat

queryParams :: err -> ByteString -> [Maybe (Word32, ByteString, Pq.Format)] -> Pq.Format -> Send err
queryParams err sql params resultFormat =
  liftPqSend err \connection -> Pq.sendQueryParams connection sql params resultFormat

pipelineSync :: err -> Send err
pipelineSync err =
  liftPqSend err \connection -> Pq.pipelineSync connection

enterPipelineMode :: err -> Send err
enterPipelineMode err =
  liftPqSend err \connection -> Pq.enterPipelineMode connection

exitPipelineMode :: err -> Send err
exitPipelineMode err =
  liftPqSend err \connection -> Pq.exitPipelineMode connection
