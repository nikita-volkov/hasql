-- |
-- An API of low-level IO operations.
module Hasql.IO where

import Hasql.Commands qualified as Commands
import Hasql.Contexts.Roundtrip qualified as Roundtrip
import Hasql.Decoders.Results qualified as ResultsDecoders
import Hasql.Encoders.Params qualified as ParamsEncoders
import Hasql.Errors
import Hasql.LibPq14 qualified as Pq
import Hasql.Prelude
import Hasql.PreparedStatementRegistry qualified as PreparedStatementRegistry

{-# INLINE acquireConnection #-}
acquireConnection :: ByteString -> IO Pq.Connection
acquireConnection =
  Pq.connectdb

{-# INLINE acquirePreparedStatementRegistry #-}
acquirePreparedStatementRegistry :: IO PreparedStatementRegistry.PreparedStatementRegistry
acquirePreparedStatementRegistry =
  PreparedStatementRegistry.new

{-# INLINE releaseConnection #-}
releaseConnection :: Pq.Connection -> IO ()
releaseConnection connection =
  Pq.finish connection

{-# INLINE checkConnectionStatus #-}
checkConnectionStatus :: Pq.Connection -> IO (Maybe (Maybe ByteString))
checkConnectionStatus c =
  do
    s <- Pq.status c
    case s of
      Pq.ConnectionOk -> return Nothing
      _ -> fmap Just (Pq.errorMessage c)

{-# INLINE checkServerVersion #-}
checkServerVersion :: Pq.Connection -> IO (Maybe Int)
checkServerVersion c =
  fmap (mfilter (< 80200) . Just) (Pq.serverVersion c)

{-# INLINE getIntegerDatetimes #-}
getIntegerDatetimes :: Pq.Connection -> IO Bool
getIntegerDatetimes c =
  fmap decodeValue $ Pq.parameterStatus c "integer_datetimes"
  where
    decodeValue =
      \case
        Just "on" -> True
        _ -> False

{-# INLINE initConnection #-}
initConnection :: Pq.Connection -> IO ()
initConnection c =
  void $ Pq.exec c (Commands.asBytes (Commands.setEncodersToUTF8 <> Commands.setMinClientMessagesToWarning))

{-# INLINE getResults #-}
getResults :: Pq.Connection -> Bool -> ResultsDecoders.Results a -> IO (Either CommandError a)
getResults connection integerDatetimes decoder =
  {-# SCC "getResults" #-}
  (<*) <$> get <*> dropRemainders
  where
    get =
      ResultsDecoders.run decoder connection integerDatetimes
    dropRemainders =
      ResultsDecoders.run ResultsDecoders.dropRemainders connection integerDatetimes

{-# INLINE getPreparedStatementKey #-}
getPreparedStatementKey ::
  Pq.Connection ->
  PreparedStatementRegistry.PreparedStatementRegistry ->
  ByteString ->
  [Pq.Oid] ->
  IO (Either CommandError ByteString)
getPreparedStatementKey connection registry template oidList =
  {-# SCC "getPreparedStatementKey" #-}
  PreparedStatementRegistry.update localKey onNewRemoteKey onOldRemoteKey registry
  where
    localKey =
      PreparedStatementRegistry.LocalKey template oidList
    onNewRemoteKey key = do
      recv <- Roundtrip.run (Roundtrip.prepare key template oidList) connection
      result <- recv
      case result of
        Left x -> pure (False, Left x)
        Right _ -> pure (True, Right key)
    onOldRemoteKey key =
      pure (pure key)

{-# INLINE checkedSend #-}
checkedSend :: Pq.Connection -> IO Bool -> IO (Either CommandError ())
checkedSend connection send =
  send >>= \case
    False -> fmap (Left . ClientError) $ Pq.errorMessage connection
    True -> pure (Right ())

{-# INLINE sendPreparedParametricStatement #-}
sendPreparedParametricStatement ::
  Pq.Connection ->
  PreparedStatementRegistry.PreparedStatementRegistry ->
  Bool ->
  ByteString ->
  ParamsEncoders.Params a ->
  a ->
  IO (Either CommandError ())
sendPreparedParametricStatement connection registry integerDatetimes template encoder input =
  runExceptT $ do
    key <- ExceptT $ getPreparedStatementKey connection registry template oidList
    ExceptT $ checkedSend connection $ Pq.sendQueryPrepared connection key valueAndFormatList Pq.Binary
  where
    (oidList, valueAndFormatList) =
      ParamsEncoders.compilePreparedStatementData encoder integerDatetimes input

{-# INLINE sendUnpreparedParametricStatement #-}
sendUnpreparedParametricStatement ::
  Pq.Connection ->
  Bool ->
  ByteString ->
  ParamsEncoders.Params a ->
  a ->
  IO (Either CommandError ())
sendUnpreparedParametricStatement connection integerDatetimes template encoder input =
  checkedSend connection
    $ Pq.sendQueryParams
      connection
      template
      (ParamsEncoders.compileUnpreparedStatementData encoder integerDatetimes input)
      Pq.Binary

{-# INLINE sendParametricStatement #-}
sendParametricStatement ::
  Pq.Connection ->
  Bool ->
  PreparedStatementRegistry.PreparedStatementRegistry ->
  ByteString ->
  ParamsEncoders.Params a ->
  Bool ->
  a ->
  IO (Either CommandError ())
sendParametricStatement connection integerDatetimes registry template encoder prepared params =
  {-# SCC "sendParametricStatement" #-}
  if prepared
    then sendPreparedParametricStatement connection registry integerDatetimes template encoder params
    else sendUnpreparedParametricStatement connection integerDatetimes template encoder params

{-# INLINE sendNonparametricStatement #-}
sendNonparametricStatement :: Pq.Connection -> ByteString -> IO (Either CommandError ())
sendNonparametricStatement connection sql =
  checkedSend connection $ Pq.sendQuery connection sql
