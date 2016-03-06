-- |
-- An API of low-level IO operations.
module Hasql.Private.IO
where

import Hasql.Prelude
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Hasql.Commands as Commands
import qualified Hasql.Private.PreparedStatementRegistry as PreparedStatementRegistry
import qualified Hasql.Decoders.Result as ResultDecoders
import qualified Hasql.Decoders.Results as ResultsDecoders
import qualified Hasql.Encoders.Params as ParamsEncoders
import qualified Data.DList as DList


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
getResults :: LibPQ.Connection -> Bool -> ResultsDecoders.Results a -> IO (Either ResultsDecoders.Error a)
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
  LibPQ.Connection -> PreparedStatementRegistry.PreparedStatementRegistry ->
  ByteString -> [LibPQ.Oid] ->
  IO (Either ResultsDecoders.Error ByteString)
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
                then ResultsDecoders.single ResultDecoders.unit
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
checkedSend :: LibPQ.Connection -> IO Bool -> IO (Either ResultsDecoders.Error ())
checkedSend connection send =
  send >>= \case
    False -> fmap (Left . ResultsDecoders.ClientError) $ LibPQ.errorMessage connection
    True -> pure (Right ())

{-# INLINE sendPreparedParametricQuery #-}
sendPreparedParametricQuery ::
  LibPQ.Connection ->
  PreparedStatementRegistry.PreparedStatementRegistry ->
  ByteString ->
  [LibPQ.Oid] ->
  [Maybe (ByteString, LibPQ.Format)] ->
  IO (Either ResultsDecoders.Error ())
sendPreparedParametricQuery connection registry template oidList valueAndFormatList =
  runEitherT $ do
    key <- EitherT $ getPreparedStatementKey connection registry template oidList
    EitherT $ checkedSend connection $ LibPQ.sendQueryPrepared connection key valueAndFormatList LibPQ.Binary

{-# INLINE sendUnpreparedParametricQuery #-}
sendUnpreparedParametricQuery ::
  LibPQ.Connection ->
  ByteString ->
  [Maybe (LibPQ.Oid, ByteString, LibPQ.Format)] ->
  IO (Either ResultsDecoders.Error ())
sendUnpreparedParametricQuery connection template paramList =
  checkedSend connection $ LibPQ.sendQueryParams connection template paramList LibPQ.Binary

{-# INLINE sendParametricQuery #-}
sendParametricQuery ::
  LibPQ.Connection ->
  Bool -> 
  PreparedStatementRegistry.PreparedStatementRegistry ->
  ByteString ->
  ParamsEncoders.Params a ->
  Bool ->
  a ->
  IO (Either ResultsDecoders.Error ())
sendParametricQuery connection integerDatetimes registry template encoder prepared params =
  {-# SCC "sendParametricQuery" #-} 
  if prepared
    then
      let
        (oidList, valueAndFormatList) =
          ParamsEncoders.run' encoder params integerDatetimes
        in
          sendPreparedParametricQuery connection registry template oidList valueAndFormatList
    else
      let
        paramList =
          ParamsEncoders.run'' encoder params integerDatetimes
        in
          sendUnpreparedParametricQuery connection template paramList

{-# INLINE sendNonparametricQuery #-}
sendNonparametricQuery :: LibPQ.Connection -> ByteString -> IO (Either ResultsDecoders.Error ())
sendNonparametricQuery connection sql =
  checkedSend connection $ LibPQ.sendQuery connection sql
