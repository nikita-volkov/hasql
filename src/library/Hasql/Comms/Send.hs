module Hasql.Comms.Send where

import Hasql.Platform.Prelude
import Pqi qualified

data Result context
  = Ok
  | Error context (Maybe ByteString)
  deriving stock (Eq, Show, Functor)

newtype Send context
  = Send (forall conn. (Pqi.IsConnection conn) => conn -> IO (Result context))

instance Functor Send where
  fmap f (Send g) = Send $ \conn -> fmap (fmap f) (g conn)

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

toHandler :: (Pqi.IsConnection conn) => Send context -> conn -> IO (Result context)
toHandler (Send send) = send

liftPqiSend :: context -> (forall conn. (Pqi.IsConnection conn) => conn -> IO Bool) -> Send context
liftPqiSend context pqiSend = Send \connection -> do
  success <- pqiSend connection
  if success
    then pure Ok
    else do
      errorMessage <- Pqi.errorMessage connection
      pure (Error context errorMessage)

prepare :: context -> ByteString -> ByteString -> Maybe [Word32] -> Send context
prepare context statementName sql oidList =
  liftPqiSend context \connection -> Pqi.sendPrepare connection statementName sql oidList

query :: context -> ByteString -> Send context
query context sql =
  liftPqiSend context \connection -> Pqi.sendQuery connection sql

queryPrepared :: context -> ByteString -> [Maybe (ByteString, Pqi.Format)] -> Pqi.Format -> Send context
queryPrepared context statementName params resultFormat =
  liftPqiSend context \connection -> Pqi.sendQueryPrepared connection statementName params resultFormat

queryParams :: context -> ByteString -> [Maybe (Word32, ByteString, Pqi.Format)] -> Pqi.Format -> Send context
queryParams context sql params resultFormat =
  liftPqiSend context \connection -> Pqi.sendQueryParams connection sql params resultFormat

pipelineSync :: context -> Send context
pipelineSync context =
  liftPqiSend context \connection -> Pqi.pipelineSync connection

enterPipelineMode :: context -> Send context
enterPipelineMode context =
  liftPqiSend context \connection -> Pqi.enterPipelineMode connection

exitPipelineMode :: context -> Send context
exitPipelineMode context =
  liftPqiSend context \connection -> Pqi.exitPipelineMode connection
