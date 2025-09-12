module Core.Contexts.Command where

import Core.Contexts.ResultConsumer qualified as ResultConsumer
import Core.Errors
import Core.Structures.StatementCache qualified as StatementCache
import Platform.Prelude
import Pq qualified

-- | Statically structured command series execution context.
--
-- All commands get executed with errors being accumulated and reported at the end.
-- Errors do not interrupt sending or reception of other commands.
newtype Command a
  = Command {run :: Pq.Connection -> IO (Either CommandError a)}
  deriving stock (Functor)

instance Applicative Command where
  {-# INLINE pure #-}
  pure x = Command \_ -> pure (Right x)
  {-# INLINE (<*>) #-}
  Command send1 <*> Command send2 =
    Command \cs -> do
      ef <- send1 cs
      eg <- send2 cs
      pure (ef <*> eg)

runSql :: ByteString -> Command ()
runSql sql = do
  sendQuery sql
  consumeResult ResultConsumer.ok
  drainResults
  pure ()

liftPqCommand :: (Pq.Connection -> IO Bool) -> Command ()
liftPqCommand command =
  Command \connection -> do
    success <- command connection
    if success
      then pure (Right ())
      else do
        errorMessage <- Pq.errorMessage connection
        pure (Left (ClientError errorMessage))

-- | Consume a single result from the connection using the provided result decoder.
{-# INLINE consumeResult #-}
consumeResult :: ResultConsumer.ResultConsumer a -> Command a
consumeResult resultConsumer =
  Command \connection -> do
    resultMb <- Pq.getResult connection
    case resultMb of
      Just result -> do
        resultR <- ResultConsumer.toHandler resultConsumer result
        case resultR of
          Left err -> pure (Left (ResultError err))
          Right val -> pure (Right val)
      Nothing -> do
        errorMessage <- Pq.errorMessage connection
        pure (Left (ClientError errorMessage))

-- | Drain all results from the connection, expecting all to be OK.
{-# INLINE drainResults #-}
drainResults :: Command ()
drainResults =
  Command \connection ->
    let go !output = do
          resultMb <- Pq.getResult connection
          case resultMb of
            Nothing -> pure output
            Just result -> do
              resultR <- ResultConsumer.toHandler ResultConsumer.ok result
              case resultR of
                Left err -> go (output *> Left (ResultError err))
                Right () -> go output
     in go (Right ())

sendQuery :: ByteString -> Command ()
sendQuery sql =
  liftPqCommand \connection -> do
    Pq.sendQuery connection sql

{-# INLINE sendQueryParams #-}
sendQueryParams ::
  ByteString ->
  [Maybe (Pq.Oid, ByteString, Pq.Format)] ->
  Command ()
sendQueryParams sql input =
  liftPqCommand \connection -> do
    Pq.sendQueryParams connection sql input Pq.Binary

prepare :: ByteString -> ByteString -> [Pq.Oid] -> Command ()
prepare key template oidList =
  let pqOidList = case oidList of
        [] -> Nothing
        _ -> Just oidList
   in do
        liftPqCommand \connection -> do
          Pq.sendPrepare connection key template pqOidList
        consumeResult ResultConsumer.ok
        drainResults
        pure ()

sendQueryPrepared ::
  ByteString ->
  [Maybe (ByteString, Pq.Format)] ->
  Command ()
sendQueryPrepared key params =
  liftPqCommand \connection -> do
    Pq.sendQueryPrepared connection key params Pq.Binary

prepareWithRegistry ::
  ByteString ->
  [Pq.Oid] ->
  [Maybe (ByteString, Pq.Format)] ->
  StatementCache.StatementCache ->
  Command StatementCache.StatementCache
prepareWithRegistry sql oidList valueAndFormatList registry =
  let localKey = StatementCache.LocalKey sql oidList
   in case StatementCache.lookup localKey registry of
        Just key -> do
          sendQueryPrepared key valueAndFormatList
          pure registry
        Nothing -> do
          let (key, newRegistry) = StatementCache.insert localKey registry
          prepare key sql oidList
          sendQueryPrepared key valueAndFormatList
          pure newRegistry

-- | Version that defers cache update until after execution succeeds
prepareStatementIfNeeded ::
  ByteString ->
  [Pq.Oid] ->
  [Maybe (ByteString, Pq.Format)] ->
  StatementCache.StatementCache ->
  Command (StatementCache.StatementCache, Maybe ByteString)
prepareStatementIfNeeded sql oidList valueAndFormatList registry =
  let localKey = StatementCache.LocalKey sql oidList
   in case StatementCache.lookup localKey registry of
        Just key -> do
          sendQueryPrepared key valueAndFormatList
          pure (registry, Nothing)  -- No new statement prepared
        Nothing -> do
          let (key, newRegistry) = StatementCache.insert localKey registry
          prepare key sql oidList
          sendQueryPrepared key valueAndFormatList
          pure (newRegistry, Just key)  -- New statement was prepared

-- | Try to run a command, returning Left on failure instead of failing the entire Command
tryCommand :: Command a -> Command (Either CommandError a)
tryCommand (Command action) = Command \conn -> do
  result <- action conn
  case result of
    Left err -> pure (Right (Left err))
    Right val -> pure (Right (Right val))

-- | Deallocate a prepared statement
deallocatePrepared :: ByteString -> Command ()
deallocatePrepared key = do
  liftPqCommand \connection -> 
    Pq.sendQuery connection ("DEALLOCATE " <> key)
  consumeResult ResultConsumer.ok
  drainResults

describePrepared ::
  -- | Key.
  ByteString ->
  Command [Pq.Oid]
describePrepared key = do
  liftPqCommand \connection -> do
    Pq.sendDescribePrepared connection key
  oids <- consumeResult do
    ResultConsumer.ok
    ResultConsumer.columnOids
  drainResults
  pure oids
