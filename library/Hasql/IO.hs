-- |
-- An API of low-level IO operations.
module Hasql.IO
where

import Hasql.Prelude
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Hasql.Commands as Commands
import qualified Hasql.PreparedStatementRegistry as PreparedStatementRegistry
import qualified Hasql.Decoding.Result as ResultDecoding
import qualified Hasql.Decoding.Results as ResultsDecoding
import qualified Hasql.Encoding.Params as ParamsEncoding
import qualified Hasql.Settings as Settings
import qualified Data.DList as DList


{-# INLINE acquireConnection #-}
acquireConnection :: Settings.Settings -> IO LibPQ.Connection
acquireConnection settings =
  LibPQ.connectdb (Settings.asBytes settings)

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
  void $ LibPQ.exec c (Commands.asBytes (Commands.setEncodingToUTF8 <> Commands.setMinClientMessagesToWarning))

{-# INLINE getResults #-}
getResults :: LibPQ.Connection -> Bool -> ResultsDecoding.Results a -> IO (Either ResultsDecoding.Error a)
getResults connection integerDatetimes des =
  {-# SCC "getResults" #-} 
  ResultsDecoding.run (des <* ResultsDecoding.dropRemainders) (integerDatetimes, connection)

{-# INLINE getPreparedStatementKey #-}
getPreparedStatementKey ::
  LibPQ.Connection -> PreparedStatementRegistry.PreparedStatementRegistry ->
  ByteString -> [LibPQ.Oid] ->
  IO (Either ResultsDecoding.Error ByteString)
getPreparedStatementKey connection registry template oidList =
  {-# SCC "getPreparedStatementKey" #-} 
  do
    keyMaybe <- PreparedStatementRegistry.lookup template wordOIDList registry
    case keyMaybe of
      Just key ->
        pure (pure key)
      Nothing -> 
        do
          key <- PreparedStatementRegistry.register template wordOIDList registry
          sent <- LibPQ.sendPrepare connection key template (mfilter (not . null) (Just oidList))
          let resultsDecoder = 
                if sent
                  then ResultsDecoding.single ResultDecoding.unit
                  else ResultsDecoding.clientError
          runEitherT $ do
            EitherT $ getResults connection undefined resultsDecoder
            pure key
  where
    wordOIDList =
      map (\(LibPQ.Oid x) -> fromIntegral x) oidList

{-# INLINE checkedSend #-}
checkedSend :: LibPQ.Connection -> IO Bool -> IO (Either ResultsDecoding.Error ())
checkedSend connection send =
  send >>= \case
    False -> fmap (Left . ResultsDecoding.ClientError) $ LibPQ.errorMessage connection
    True -> pure (Right ())

{-# INLINE sendPreparedParametricQuery #-}
sendPreparedParametricQuery ::
  LibPQ.Connection ->
  PreparedStatementRegistry.PreparedStatementRegistry ->
  ByteString ->
  [LibPQ.Oid] ->
  [Maybe (ByteString, LibPQ.Format)] ->
  IO (Either ResultsDecoding.Error ())
sendPreparedParametricQuery connection registry template oidList valueAndFormatList =
  runEitherT $ do
    key <- EitherT $ getPreparedStatementKey connection registry template oidList
    EitherT $ checkedSend connection $ LibPQ.sendQueryPrepared connection key valueAndFormatList LibPQ.Binary

{-# INLINE sendUnpreparedParametricQuery #-}
sendUnpreparedParametricQuery ::
  LibPQ.Connection ->
  ByteString ->
  [Maybe (LibPQ.Oid, ByteString, LibPQ.Format)] ->
  IO (Either ResultsDecoding.Error ())
sendUnpreparedParametricQuery connection template paramList =
  checkedSend connection $ LibPQ.sendQueryParams connection template paramList LibPQ.Binary

{-# INLINE sendParametricQuery #-}
sendParametricQuery ::
  LibPQ.Connection ->
  Bool -> 
  PreparedStatementRegistry.PreparedStatementRegistry ->
  ByteString ->
  ParamsEncoding.Params a ->
  Bool ->
  a ->
  IO (Either ResultsDecoding.Error ())
sendParametricQuery connection integerDatetimes registry template encoder prepared params =
  {-# SCC "sendParametricQuery" #-} 
  if prepared
    then
      let
        (oidList, valueAndFormatList) =
          ParamsEncoding.run' encoder params integerDatetimes
        in
          sendPreparedParametricQuery connection registry template oidList valueAndFormatList
    else
      let
        paramList =
          ParamsEncoding.run'' encoder params integerDatetimes
        in
          sendUnpreparedParametricQuery connection template paramList

{-# INLINE sendNonparametricQuery #-}
sendNonparametricQuery :: LibPQ.Connection -> ByteString -> IO (Either ResultsDecoding.Error ())
sendNonparametricQuery connection sql =
  checkedSend connection $ LibPQ.sendQuery connection sql
