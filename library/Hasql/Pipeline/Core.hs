module Hasql.Pipeline.Core where

import Hasql.Decoders.All qualified as Decoders
import Hasql.Decoders.Result qualified as Decoders.Result
import Hasql.Decoders.Results qualified as Decoders.Results
import Hasql.Encoders.All qualified as Encoders
import Hasql.Encoders.Params qualified as Encoders.Params
import Hasql.Errors
import Hasql.LibPq14 qualified as Pq
import Hasql.Prelude
import Hasql.PreparedStatementRegistry qualified as PreparedStatementRegistry
import Hasql.Statement qualified as Statement

run :: forall a. Pipeline a -> Pq.Connection -> PreparedStatementRegistry.PreparedStatementRegistry -> Bool -> IO (Either SessionError a)
run (Pipeline sendQueriesInIO) connection registry integerDatetimes = do
  runExceptT do
    enterPipelineMode
    recvQueries <- sendQueries
    pipelineSync
    finallyE recvQueries do
      recvPipelineSync
      exitPipelineMode
  where
    enterPipelineMode :: ExceptT SessionError IO ()
    enterPipelineMode =
      runCommand $ Pq.enterPipelineMode connection

    exitPipelineMode :: ExceptT SessionError IO ()
    exitPipelineMode =
      runCommand $ Pq.exitPipelineMode connection

    sendQueries :: ExceptT SessionError IO (ExceptT SessionError IO a)
    sendQueries =
      fmap ExceptT $ ExceptT $ sendQueriesInIO connection registry integerDatetimes

    pipelineSync :: ExceptT SessionError IO ()
    pipelineSync =
      runCommand $ Pq.pipelineSync connection

    recvPipelineSync :: ExceptT SessionError IO ()
    recvPipelineSync =
      runResultsDecoder
        $ Decoders.Results.single Decoders.Result.pipelineSync

    runResultsDecoder :: forall a. Decoders.Results.Results a -> ExceptT SessionError IO a
    runResultsDecoder decoder =
      ExceptT
        $ fmap (first PipelineError)
        $ Decoders.Results.run decoder connection integerDatetimes

    runCommand :: IO Bool -> ExceptT SessionError IO ()
    runCommand action =
      lift action >>= \case
        True -> pure ()
        False -> ExceptT (Left . PipelineError . ClientError <$> Pq.errorMessage connection)

-- |
-- Abstraction over the pipelining mode of execution of queries.
--
-- It allows you to issue multiple queries to the server in much fewer network transactions.
-- If the amounts of sent and received data do not surpass the buffer sizes it will be just a single roundtrip.
-- Usually the buffer size is 8KB.
--
-- This execution mode is much more efficient than running queries directly from 'Hasql.Session.Session', because in session every statement execution involves a dedicated network roundtrip.
-- An obvious question rises then: why not execute all queries like that?
--
-- In situations where the parameters depend on the result of another query it is impossible to execute them in parallel, because the client needs to receive the results of one query before sending the request to execute the next query.
-- This reasoning is essentially the same as the one for the difference between 'Applicative' and 'Monad'.
-- That\'s why 'Pipeline' does not have the 'Monad' instance.
--
-- To execute 'Pipeline' lift it into 'Session' via 'Hasql.Session.pipeline'.
--
-- __Attention__: using this feature requires \"libpq\" of version >14.
newtype Pipeline a
  = Pipeline
      ( Pq.Connection ->
        PreparedStatementRegistry.PreparedStatementRegistry ->
        Bool ->
        IO (Either SessionError (IO (Either SessionError a)))
      )
  deriving (Functor)

instance Applicative Pipeline where
  pure a =
    Pipeline (\_ _ _ -> pure (Right (pure (Right a))))

  Pipeline lSend <*> Pipeline rSend =
    Pipeline \conn reg integerDatetimes ->
      lSend conn reg integerDatetimes >>= \case
        Left sendErr ->
          pure (Left sendErr)
        Right lRecv ->
          rSend conn reg integerDatetimes <&> \case
            Left sendErr ->
              Left sendErr
            Right rRecv ->
              Right (liftA2 (<*>) lRecv rRecv)

-- |
-- Execute a statement in pipelining mode.
statement :: params -> Statement.Statement params result -> Pipeline result
statement params (Statement.Statement sql (Encoders.Params encoder) (Decoders.Result decoder) preparable) =
  Pipeline run
  where
    run connection registry integerDatetimes =
      if preparable
        then runPrepared
        else runUnprepared
      where
        runPrepared = runExceptT do
          (key, keyRecv) <- ExceptT resolvePreparedStatementKey
          queryRecv <- ExceptT (sendQuery key)
          pure (keyRecv *> queryRecv)
          where
            (oidList, valueAndFormatList) =
              Encoders.Params.compilePreparedStatementData encoder integerDatetimes params

            resolvePreparedStatementKey =
              PreparedStatementRegistry.update localKey onNewRemoteKey onOldRemoteKey registry
              where
                localKey =
                  PreparedStatementRegistry.LocalKey sql oidList
                onNewRemoteKey key =
                  do
                    sent <- Pq.sendPrepare connection key sql (mfilter (not . null) (Just oidList))
                    if sent
                      then pure (True, Right (key, recv))
                      else (False,) . Left . commandToSessionError . ClientError <$> Pq.errorMessage connection
                  where
                    recv =
                      fmap (first commandToSessionError)
                        $ (<*)
                        <$> Decoders.Results.run (Decoders.Results.single Decoders.Result.noResult) connection integerDatetimes
                        <*> Decoders.Results.run Decoders.Results.dropRemainders connection integerDatetimes
                onOldRemoteKey key =
                  pure (Right (key, pure (Right ())))

            sendQuery key =
              Pq.sendQueryPrepared connection key valueAndFormatList Pq.Binary >>= \case
                False -> Left . commandToSessionError . ClientError <$> Pq.errorMessage connection
                True -> pure (Right recv)
              where
                recv =
                  fmap (first commandToSessionError)
                    $ (<*)
                    <$> Decoders.Results.run decoder connection integerDatetimes
                    <*> Decoders.Results.run Decoders.Results.dropRemainders connection integerDatetimes

        runUnprepared =
          Pq.sendQueryParams connection sql (Encoders.Params.compileUnpreparedStatementData encoder integerDatetimes params) Pq.Binary >>= \case
            False -> Left . commandToSessionError . ClientError <$> Pq.errorMessage connection
            True -> pure (Right recv)
          where
            recv =
              fmap (first commandToSessionError)
                $ (<*)
                <$> Decoders.Results.run decoder connection integerDatetimes
                <*> Decoders.Results.run Decoders.Results.dropRemainders connection integerDatetimes

    commandToSessionError =
      QueryError sql (Encoders.Params.renderReadable encoder params)
