module Hipq.Send where

import Platform.Prelude
import Pq qualified

data Result context
  = Ok
  | Error context (Maybe ByteString)

newtype Send context
  = Send (Pq.Connection -> IO (Result context))

instance Semigroup (Send context) where
  {-# INLINE (<>) #-}
  Send send1 <> Send send2 = Send \cs -> do
    result <- send1 cs
    case result of
      Error context details -> pure (Error context details)
      Ok -> do
        result2 <- send2 cs
        pure result2

instance Monoid (Send context) where
  {-# INLINE mempty #-}
  mempty = Send \_ -> pure Ok

toHandler :: Send context -> Pq.Connection -> IO (Result context)
toHandler (Send send) = send

liftPqSend :: context -> (Pq.Connection -> IO Bool) -> Send context
liftPqSend context pqSend = Send \connection -> do
  success <- pqSend connection
  if success
    then pure Ok
    else do
      errorMessage <- Pq.errorMessage connection
      pure (Error context errorMessage)

prepare :: context -> ByteString -> ByteString -> Maybe [Pq.Oid] -> Send context
prepare context statementName sql oidList =
  liftPqSend context \connection -> Pq.sendPrepare connection statementName sql oidList

query :: context -> ByteString -> Send context
query context sql =
  liftPqSend context \connection -> Pq.sendQuery connection sql

queryPrepared :: context -> ByteString -> [Maybe (ByteString, Pq.Format)] -> Pq.Format -> Send context
queryPrepared context statementName params resultFormat =
  liftPqSend context \connection -> Pq.sendQueryPrepared connection statementName params resultFormat

queryParams :: context -> ByteString -> [Maybe (Pq.Oid, ByteString, Pq.Format)] -> Pq.Format -> Send context
queryParams context sql params resultFormat =
  liftPqSend context \connection -> Pq.sendQueryParams connection sql params resultFormat

pipelineSync :: context -> Send context
pipelineSync context =
  liftPqSend context \connection -> Pq.pipelineSync connection

enterPipelineMode :: context -> Send context
enterPipelineMode context =
  liftPqSend context \connection -> Pq.enterPipelineMode connection

exitPipelineMode :: context -> Send context
exitPipelineMode context =
  liftPqSend context \connection -> Pq.exitPipelineMode connection
