module Hasql.Comms.Send where

import Hasql.Driver.Interface qualified as Interface
import Hasql.Platform.Prelude

data Result context
  = Ok
  | Error context (Maybe ByteString)
  deriving stock (Eq, Show, Functor)

newtype Send conn context
  = Send (conn -> IO (Result context))
  deriving stock (Functor)

instance Semigroup (Send conn context) where
  {-# INLINE (<>) #-}
  Send send1 <> Send send2 = Send \cs -> do
    result <- send1 cs
    case result of
      Error context details -> pure (Error context details)
      Ok -> do
        result2 <- send2 cs
        pure result2

instance Monoid (Send conn context) where
  {-# INLINE mempty #-}
  mempty = Send \_ -> pure Ok

toHandler :: Send conn context -> conn -> IO (Result context)
toHandler (Send send) = send

liftSend :: context -> (conn -> IO Bool) -> (conn -> IO (Maybe ByteString)) -> Send conn context
liftSend context sendFn getErrorMessage = Send \connection -> do
  success <- sendFn connection
  if success
    then pure Ok
    else do
      errorMessage <- getErrorMessage connection
      pure (Error context errorMessage)

prepare :: Interface.Driver conn result -> context -> ByteString -> ByteString -> [Word32] -> Send conn context
prepare drv context statementName sql oidList =
  liftSend context
    (\connection -> Interface.driverSendPrepare drv connection statementName sql oidList)
    (Interface.driverErrorMessage drv)

query :: Interface.Driver conn result -> context -> ByteString -> Send conn context
query drv context sql =
  liftSend context
    (\connection -> Interface.driverSendQuery drv connection sql)
    (Interface.driverErrorMessage drv)

queryPrepared :: Interface.Driver conn result -> context -> ByteString -> [Maybe (ByteString, Bool)] -> Send conn context
queryPrepared drv context statementName params =
  liftSend context
    (\connection -> Interface.driverSendQueryPrepared drv connection statementName params)
    (Interface.driverErrorMessage drv)

queryParams :: Interface.Driver conn result -> context -> ByteString -> [Maybe (Word32, ByteString, Bool)] -> Send conn context
queryParams drv context sql params =
  liftSend context
    (\connection -> Interface.driverSendQueryParams drv connection sql params)
    (Interface.driverErrorMessage drv)

pipelineSync :: Interface.Driver conn result -> context -> Send conn context
pipelineSync drv context =
  liftSend context
    (Interface.driverPipelineSync drv)
    (Interface.driverErrorMessage drv)

enterPipelineMode :: Interface.Driver conn result -> context -> Send conn context
enterPipelineMode drv context =
  liftSend context
    (Interface.driverEnterPipelineMode drv)
    (Interface.driverErrorMessage drv)

exitPipelineMode :: Interface.Driver conn result -> context -> Send conn context
exitPipelineMode drv context =
  liftSend context
    (Interface.driverExitPipelineMode drv)
    (Interface.driverErrorMessage drv)
