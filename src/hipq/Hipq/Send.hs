module Hipq.Send where

import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Encoding.Error qualified as Text.Encoding.Error
import Platform.Prelude
import Pq qualified

type SendError = Maybe ByteString

newtype Send
  = Send (Pq.Connection -> IO Bool)

instance Semigroup Send where
  {-# INLINE (<>) #-}
  Send send1 <> Send send2 =
    Send \cs -> do
      success1 <- send1 cs
      if success1
        then send2 cs
        else pure False

instance Monoid Send where
  {-# INLINE mempty #-}
  mempty = Send \_ -> pure True

toHandler :: Send -> Pq.Connection -> IO (Either SendError ())
toHandler (Send send) connection = do
  success <- send connection
  if success
    then pure (Right ())
    else do
      errorMessage <- Pq.errorMessage connection
      pure (Left errorMessage)

prepare :: ByteString -> ByteString -> Maybe [Pq.Oid] -> Send
prepare statementName sql oidList =
  Send \connection -> Pq.sendPrepare connection statementName sql oidList

queryPrepared :: ByteString -> [Maybe (ByteString, Pq.Format)] -> Pq.Format -> Send
queryPrepared statementName params resultFormat =
  Send \connection -> Pq.sendQueryPrepared connection statementName params resultFormat

queryParams :: ByteString -> [Maybe (Pq.Oid, ByteString, Pq.Format)] -> Pq.Format -> Send
queryParams sql params resultFormat =
  Send \connection -> Pq.sendQueryParams connection sql params resultFormat

pipelineSync :: Send
pipelineSync =
  Send \connection -> Pq.pipelineSync connection

enterPipelineMode :: Send
enterPipelineMode =
  Send \connection -> Pq.enterPipelineMode connection

exitPipelineMode :: Send
exitPipelineMode =
  Send \connection -> Pq.exitPipelineMode connection
