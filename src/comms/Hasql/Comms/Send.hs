module Hasql.Comms.Send where

import Hasql.Platform.Prelude
import Pqi qualified as Pq

-- | Outcome of sending commands: success, or a failure carrying the tag of the commands.
data Result tag
  = Ok
  | Error tag
  deriving stock (Eq, Show, Functor)

-- | An action that sends commands to a connection without receiving the results.
--
-- The @tag@ type parameter is a value attached to the commands,
-- which the error carries if sending fails.
newtype Send tag
  = Send (Pq.Connection -> IO (Result tag))
  deriving stock (Functor)

instance Semigroup (Send tag) where
  {-# INLINE (<>) #-}
  Send send1 <> Send send2 = Send \cs -> do
    result <- send1 cs
    case result of
      Error tag -> pure (Error tag)
      Ok -> do
        result2 <- send2 cs
        pure result2

instance Monoid (Send tag) where
  {-# INLINE mempty #-}
  mempty = Send \_ -> pure Ok

toHandler :: Send tag -> Pq.Connection -> IO (Result tag)
toHandler (Send send) = send

liftPqSend :: tag -> (Pq.Connection -> IO Bool) -> Send tag
liftPqSend tag pqSend = Send \connection -> do
  success <- pqSend connection
  pure (if success then Ok else Error tag)

prepare :: tag -> ByteString -> ByteString -> Maybe [Word32] -> Send tag
prepare tag statementName sql oidList =
  liftPqSend tag \connection -> Pq.sendPrepare connection statementName sql oidList

query :: tag -> ByteString -> Send tag
query tag sql =
  liftPqSend tag \connection -> Pq.sendQuery connection sql

queryPrepared :: tag -> ByteString -> [Maybe (ByteString, Pq.Format)] -> Pq.Format -> Send tag
queryPrepared tag statementName params resultFormat =
  liftPqSend tag \connection -> Pq.sendQueryPrepared connection statementName params resultFormat

queryParams :: tag -> ByteString -> [Maybe (Word32, ByteString, Pq.Format)] -> Pq.Format -> Send tag
queryParams tag sql params resultFormat =
  liftPqSend tag \connection -> Pq.sendQueryParams connection sql params resultFormat

pipelineSync :: tag -> Send tag
pipelineSync tag =
  liftPqSend tag \connection -> Pq.pipelineSync connection

enterPipelineMode :: tag -> Send tag
enterPipelineMode tag =
  liftPqSend tag \connection -> Pq.enterPipelineMode connection

exitPipelineMode :: tag -> Send tag
exitPipelineMode tag =
  liftPqSend tag \connection -> Pq.exitPipelineMode connection
