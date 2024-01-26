-- |
-- An API of low-level IO operations.
module Hasql.IO where

import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Hasql.Commands as Commands
import qualified Hasql.Decoders.Result as ResultDecoders
import qualified Hasql.Decoders.Results as ResultsDecoders
import qualified Hasql.Encoders.Params as ParamsEncoders
import Hasql.Errors
import Hasql.Prelude
import qualified Hasql.PreparedStatementRegistry as PreparedStatementRegistry

{-# INLINE acquireConnection #-}
acquireConnection :: ByteString -> IO LibPQ.Connection
acquireConnection =
  LibPQ.connectdb

{-# INLINE acquirePreparedStatementRegistry #-}
acquirePreparedStatementRegistry :: IO PreparedStatementRegistry.PreparedStatementRegistry
acquirePreparedStatementRegistry =
  PreparedStatementRegistry.new

{-# INLINE releaseConnection #-}
releaseConnection :: LibPQ.Connection -> IO ()
releaseConnection connection =
  LibPQ.finish connection

{-# INLINE checkConnectionStatus #-}
checkConnectionStatus :: LibPQ.Connection -> IO (Maybe (Maybe ByteString))
checkConnectionStatus c =
  do
    s <- LibPQ.status c
    case s of
      LibPQ.ConnectionOk -> return Nothing
      _ -> fmap Just (LibPQ.errorMessage c)

{-# INLINE checkServerVersion #-}
checkServerVersion :: LibPQ.Connection -> IO (Maybe Int)
checkServerVersion c =
  fmap (mfilter (< 80200) . Just) (LibPQ.serverVersion c)

{-# INLINE getIntegerDatetimes #-}
getIntegerDatetimes :: LibPQ.Connection -> IO Bool
getIntegerDatetimes c =
  fmap decodeValue $ LibPQ.parameterStatus c "integer_datetimes"
  where
    decodeValue =
      \case
        Just "on" -> True
        _ -> False

{-# INLINE initConnection #-}
initConnection :: LibPQ.Connection -> IO ()
initConnection c =
  void $ LibPQ.exec c (Commands.asBytes (Commands.setEncodersToUTF8 <> Commands.setMinClientMessagesToWarning))

{-# INLINE getResults #-}
getResults :: LibPQ.Connection -> Bool -> ResultsDecoders.Results a -> IO (Either CommandError a)
getResults connection integerDatetimes decoder =
  {-# SCC "getResults" #-}
  (<*) <$> get <*> dropRemainders
  where
    get =
      ResultsDecoders.run decoder (integerDatetimes, connection)
    dropRemainders =
      ResultsDecoders.run ResultsDecoders.dropRemainders (integerDatetimes, connection)

{-# INLINE getPreparedStatementKey #-}
getPreparedStatementKey ::
  LibPQ.Connection ->
  PreparedStatementRegistry.PreparedStatementRegistry ->
  ByteString ->
  [LibPQ.Oid] ->
  IO (Either CommandError ByteString)
getPreparedStatementKey connection registry template oidList =
  {-# SCC "getPreparedStatementKey" #-}
  PreparedStatementRegistry.update localKey onNewRemoteKey onOldRemoteKey registry
  where
    localKey =
      PreparedStatementRegistry.LocalKey template wordOIDList
      where
        wordOIDList =
          map (\(LibPQ.Oid x) -> fromIntegral x) oidList
    onNewRemoteKey key =
      do
        sent <- LibPQ.sendPrepare connection key template (mfilter (not . null) (Just oidList))
        let resultsDecoder =
              if sent
                then ResultsDecoders.single ResultDecoders.noResult
                else ResultsDecoders.clientError
        fmap resultsMapping $ getResults connection undefined resultsDecoder
      where
        resultsMapping =
          \case
            Left x -> (False, Left x)
            Right _ -> (True, Right key)
    onOldRemoteKey key =
      pure (pure key)

{-# INLINE checkedSend #-}
checkedSend :: LibPQ.Connection -> IO Bool -> IO (Either CommandError ())
checkedSend connection send =
  send >>= \case
    False -> fmap (Left . ClientError) $ LibPQ.errorMessage connection
    True -> pure (Right ())

{-# INLINE sendPreparedParametricStatement #-}
sendPreparedParametricStatement ::
  LibPQ.Connection ->
  PreparedStatementRegistry.PreparedStatementRegistry ->
  Bool ->
  ByteString ->
  ParamsEncoders.Params a ->
  a ->
  IO (Either CommandError ())
sendPreparedParametricStatement connection registry integerDatetimes template (ParamsEncoders.Params (Op encoderOp)) input =
  let (oidList, valueAndFormatList) =
        let step (oid, format, encoder, _) ~(oidList, bytesAndFormatList) =
              (,)
                (oid : oidList)
                (fmap (\bytes -> (bytes, format)) (encoder integerDatetimes) : bytesAndFormatList)
         in foldr step ([], []) (encoderOp input)
   in runExceptT $ do
        key <- ExceptT $ getPreparedStatementKey connection registry template oidList
        ExceptT $ checkedSend connection $ LibPQ.sendQueryPrepared connection key valueAndFormatList LibPQ.Binary

{-# INLINE sendUnpreparedParametricStatement #-}
sendUnpreparedParametricStatement ::
  LibPQ.Connection ->
  Bool ->
  ByteString ->
  ParamsEncoders.Params a ->
  a ->
  IO (Either CommandError ())
sendUnpreparedParametricStatement connection integerDatetimes template (ParamsEncoders.Params (Op encoderOp)) input =
  let params =
        let step (oid, format, encoder, _) acc =
              ((,,) <$> pure oid <*> encoder integerDatetimes <*> pure format) : acc
         in foldr step [] (encoderOp input)
   in checkedSend connection $ LibPQ.sendQueryParams connection template params LibPQ.Binary

{-# INLINE sendParametricStatement #-}
sendParametricStatement ::
  LibPQ.Connection ->
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
sendNonparametricStatement :: LibPQ.Connection -> ByteString -> IO (Either CommandError ())
sendNonparametricStatement connection sql =
  checkedSend connection $ LibPQ.sendQuery connection sql
