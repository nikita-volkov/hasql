module Hasql.Contexts.Roundtrip where

import Hasql.Contexts.ResultConsumer qualified as ResultConsumer
import Hasql.Errors
import Hasql.LibPq14 qualified as Pq
import Hasql.Prelude
import Hasql.Structures.StatementCache qualified as StatementRegistry

-- | Batch of commands executed independently, in a single roundtrip to the server, with errors interrupting neither the sending or reception, but being accumulated and reported at the end.
newtype Roundtrip a
  = Roundtrip {run :: Pq.Connection -> IO (IO (Either CommandError a))}
  deriving stock (Functor)

instance Applicative Roundtrip where
  {-# INLINE pure #-}
  pure x = Roundtrip \_ -> pure (pure (pure x))
  {-# INLINE (<*>) #-}
  Roundtrip send1 <*> Roundtrip send2 =
    Roundtrip \cs -> do
      recv1 <- send1 cs
      recv2 <- send2 cs
      pure do
        ef <- recv1
        eg <- recv2
        pure (ef <*> eg)

runSql :: ByteString -> Roundtrip ()
runSql sql = do
  sendQuery sql
  consumeResult ResultConsumer.ok
  drainResults
  pure ()

liftPqCommand :: (Pq.Connection -> IO Bool) -> Roundtrip ()
liftPqCommand command =
  Roundtrip \connection -> do
    success <- command connection
    if success
      then pure (pure (Right ()))
      else do
        errorMessage <- Pq.errorMessage connection
        pure (pure (Left (ClientError errorMessage)))

-- | Consume a single result from the connection using the provided result decoder.
{-# INLINE consumeResult #-}
consumeResult :: ResultConsumer.ResultConsumer a -> Roundtrip a
consumeResult (ResultConsumer.ResultConsumer resultHandler) =
  Roundtrip \connection -> do
    pure do
      result <- Pq.getResult connection
      case result of
        Just result -> do
          result <- resultHandler result
          case result of
            Left err -> pure (Left (ResultError err))
            Right result -> pure (Right result)
        Nothing -> do
          errorMessage <- Pq.errorMessage connection
          pure (Left (ClientError errorMessage))

-- | Drain all results from the connection, expecting all to be OK.
{-# INLINE drainResults #-}
drainResults :: Roundtrip ()
drainResults =
  Roundtrip \connection ->
    pure
      let go !output = do
            result <- Pq.getResult connection
            case result of
              Nothing -> pure (Right ())
              Just result -> do
                result <- ResultConsumer.run ResultConsumer.ok result
                case result of
                  Left err -> go (output *> Left (ResultError err))
                  Right () -> go output
       in go (Right ())

sendQuery :: ByteString -> Roundtrip ()
sendQuery sql =
  liftPqCommand \connection -> do
    Pq.sendQuery connection sql

{-# INLINE sendQueryParams #-}
sendQueryParams ::
  ByteString ->
  [Maybe (Pq.Oid, ByteString, Pq.Format)] ->
  Roundtrip ()
sendQueryParams sql input =
  liftPqCommand \connection -> do
    Pq.sendQueryParams connection sql input Pq.Binary

prepare :: ByteString -> ByteString -> [Pq.Oid] -> Roundtrip ()
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
  Roundtrip ()
sendQueryPrepared key params =
  liftPqCommand \connection -> do
    Pq.sendQueryPrepared connection key params Pq.Binary

prepareWithRegistry ::
  ByteString ->
  [Pq.Oid] ->
  [Maybe (ByteString, Pq.Format)] ->
  StatementRegistry.StatementCache ->
  Roundtrip StatementRegistry.StatementCache
prepareWithRegistry sql oidList valueAndFormatList registry =
  let localKey = StatementRegistry.LocalKey sql oidList
   in case StatementRegistry.lookup localKey registry of
        Just key -> do
          sendQueryPrepared key valueAndFormatList
          pure registry
        Nothing -> do
          let (key, newRegistry) = StatementRegistry.insert localKey registry
          prepare key sql oidList
          sendQueryPrepared key valueAndFormatList
          pure newRegistry
