module Core.Contexts.Pipeline where

import Core.Contexts.Command qualified as Command
import Core.Contexts.ParamsEncoder qualified as ParamsEncoder
import Core.Contexts.ResultConsumer qualified as ResultConsumer
import Core.Contexts.ResultDecoder qualified as ResultDecoder
import Core.Contexts.ResultsDecoder qualified as ResultsDecoder
import Core.Errors
import Core.Structures.StatementCache qualified as StatementCache
import Platform.Prelude
import Pq qualified

type Run = ExceptT SessionError (StateT StatementCache.StatementCache IO)

run :: forall a. Pipeline a -> Bool -> Pq.Connection -> StatementCache.StatementCache -> IO (Either SessionError a, StatementCache.StatementCache)
run (Pipeline sendQueriesInIO) usePreparedStatements connection cache = do
  flip runStateT cache $ runExceptT $ do
    enterPipelineMode
    recvQueries <- sendQueries
    pipelineSync
    finallyE recvQueries do
      recvPipelineSync
      exitPipelineMode
  where
    enterPipelineMode :: Run ()
    enterPipelineMode =
      runCommand $ Pq.enterPipelineMode connection

    exitPipelineMode :: Run ()
    exitPipelineMode =
      runCommand $ Pq.exitPipelineMode connection

    sendQueries :: Run (Run a)
    sendQueries = do
      cache <- get
      res <- liftIO do
        sendQueriesInIO usePreparedStatements connection cache
      (recv, newCache) <- case res of
        Left err -> throwError err
        Right ok -> pure ok
      put newCache
      pure do
        res <- liftIO recv
        case res of
          Left err -> do
            liftIO $ putStrLn $ "recvQueries failed with: " <> show err
            throwError err
          Right ok -> do
            liftIO $ putStrLn $ "recvQueries succeeded"
            pure ok

    pipelineSync :: Run ()
    pipelineSync = do
      -- DEBUG: Add tracing
      liftIO $ putStrLn "pipelineSync"
      runCommand $ Pq.pipelineSync connection

    recvPipelineSync :: Run ()
    recvPipelineSync = do
      -- DEBUG: Add tracing
      liftIO $ putStrLn "recvPipelineSync"
      runResultsDecoder
        $ ResultsDecoder.single ResultDecoder.pipelineSyncOrAbort

    runResultsDecoder :: forall a. ResultsDecoder.ResultsDecoder a -> Run a
    runResultsDecoder decoder = do
      res <- liftIO do
        ResultsDecoder.toHandler decoder connection
      case res of
        Right ok -> pure ok
        Left err -> throwError (PipelineError err)

    runCommand :: IO Bool -> Run ()
    runCommand action =
      liftIO action >>= \case
        True -> pure ()
        False -> do
          err <- liftIO do
            Pq.errorMessage connection
          throwError . PipelineError . ClientError $ err

-- |
-- Composable abstraction over the execution of queries in [the pipeline mode](https://www.postgresql.org/docs/current/libpq-pipeline-mode.html).
--
-- It allows you to issue multiple queries to the server in much fewer network transactions.
-- If the amounts of sent and received data do not surpass the buffer sizes in the driver and on the server it will be just a single roundtrip.
-- Typically the buffer size is 8KB.
--
-- This execution mode is much more efficient than running queries directly from 'Hasql.Session.Session', because in session every statement execution involves a dedicated network roundtrip.
--
-- An obvious question rises then: why not execute all queries like that?
-- In situations where the parameters depend on the result of another query it is impossible to execute them in parallel, because the client needs to receive the results of one query before sending the request to execute the next.
-- This reasoning is essentially the same as the one for the difference between 'Applicative' and 'Monad'.
-- That\'s why 'Pipeline' does not have the 'Monad' instance.
--
-- To execute 'Pipeline' lift it into 'Hasql.Session.Session' via 'Hasql.Session.pipeline'.
--
-- == Examples
--
-- === Insert-Many or Batch-Insert
--
-- You can use pipeline to turn a single-row insert query into an efficient multi-row insertion session.
-- In effect this should be comparable in performance to issuing a single multi-row insert statement.
--
-- Given the following definition in a Statements module:
--
-- @
-- insertOrder :: 'Hasql.Statement.Statement' OrderDetails OrderId
-- @
--
-- You can lift it into the following session
--
-- @
-- insertOrders :: [OrderDetails] -> 'Hasql.Session.Session' [OrderId]
-- insertOrders orders =
--   'Hasql.Session.pipeline' $
--     for orders $ \\order ->
--       'Hasql.Pipeline.statement' order Statements.insertOrder
-- @
--
-- === Combining Queries
--
-- Given the following definitions in a Statements module:
--
-- @
-- selectOrderDetails :: 'Hasql.Statement.Statement' OrderId (Maybe OrderDetails)
-- selectOrderProducts :: 'Hasql.Statement.Statement' OrderId [OrderProduct]
-- selectOrderFinancialTransactions :: 'Hasql.Statement.Statement' OrderId [FinancialTransaction]
-- @
--
-- You can combine them into a session using the `ApplicativeDo` extension as follows:
--
-- @
-- selectEverythingAboutOrder :: OrderId -> 'Hasql.Session.Session' (Maybe OrderDetails, [OrderProduct], [FinancialTransaction])
-- selectEverythingAboutOrder orderId =
--   'Hasql.Session.pipeline' $ do
--     details <- 'Hasql.Pipeline.statement' orderId Statements.selectOrderDetails
--     products <- 'Hasql.Pipeline.statement' orderId Statements.selectOrderProducts
--     transactions <- 'Hasql.Pipeline.statement' orderId Statements.selectOrderFinancialTransactions
--     pure (details, products, transactions)
-- @
newtype Pipeline a
  = Pipeline
      ( Bool ->
        Pq.Connection ->
        StatementCache.StatementCache ->
        IO (Either SessionError (IO (Either SessionError a), StatementCache.StatementCache))
      )
  deriving (Functor)

instance Applicative Pipeline where
  pure a =
    Pipeline (\_ _ cache -> pure (Right (pure (Right a), cache)))

  Pipeline lSend <*> Pipeline rSend =
    Pipeline \usePreparedStatements conn cache ->
      lSend usePreparedStatements conn cache >>= \case
        Left sendErr ->
          pure (Left sendErr)
        Right (lRecv, cache1) ->
          rSend usePreparedStatements conn cache1 <&> \case
            Left sendErr ->
              Left sendErr
            Right (rRecv, cache2) ->
              Right (liftA2 (<*>) lRecv rRecv, cache2)

-- |
-- Execute a statement in pipelining mode.
statement :: ByteString -> ParamsEncoder.ParamsEncoder params -> ResultsDecoder.ResultsDecoder result -> Bool -> params -> Pipeline result
statement sql encoder decoder preparable params =
  Pipeline run
  where
    run usePreparedStatements connection cache =
      if usePreparedStatements && preparable
        then runPrepared
        else runUnprepared
      where
        runPrepared = runExceptT do
          (key, keyRecv, newCache) <- ExceptT resolvePreparedStatementKey
          queryRecv <- ExceptT (sendQuery key)
          let combinedRecv = do
                keyResult <- keyRecv
                case keyResult of
                  Left err -> do
                    putStrLn $ "keyRecv failed with: " <> show err
                    pure (Left err)
                  Right _ -> do
                    putStrLn "keyRecv succeeded, executing queryRecv"
                    queryRecv
          pure (combinedRecv, newCache)
          where
            (oidList, valueAndFormatList) =
              ParamsEncoder.compilePreparedStatementData encoder params

            resolvePreparedStatementKey =
              case StatementCache.lookup localKey cache of
                Just remoteKey -> pure (Right (remoteKey, pure (Right ()), cache))
                Nothing -> do
                  let (remoteKey, newCache) = StatementCache.insert localKey cache
                  sent <- Pq.sendPrepare connection remoteKey sql (mfilter (not . null) (Just oidList))
                  -- DEBUG: Add tracing
                  liftIO $ putStrLn $ "sendPrepare: " <> show remoteKey <> " for LocalKey " <> show sql <> " " <> show oidList
                  if sent
                    then pure (Right (remoteKey, recv, newCache))
                    else do
                      errMsg <- Pq.errorMessage connection
                      pure (Left (commandToSessionError (ClientError errMsg)))
                  where
                    recv = do
                      putStrLn "executing keyRecv for prepare"
                      fmap (first commandToSessionError)
                        $ Command.run command connection
                      where
                        command = do
                          Command.consumeResult ResultConsumer.ok
                          Command.drainResults
                          pure ()
              where
                localKey =
                  StatementCache.LocalKey sql oidList

            sendQuery key =
              Pq.sendQueryPrepared connection key valueAndFormatList Pq.Binary >>= \case
                False -> Left . commandToSessionError . ClientError <$> Pq.errorMessage connection
                True -> pure (Right recv)
              where
                recv = do
                  putStrLn "executing queryRecv for prepared statement"
                  fmap (first commandToSessionError)
                    $ (<*)
                    <$> ResultsDecoder.toHandler decoder connection
                    <*> ResultsDecoder.toHandler ResultsDecoder.dropRemainders connection

        runUnprepared = do
          sent <- Pq.sendQueryParams connection sql (ParamsEncoder.compileUnpreparedStatementData encoder params) Pq.Binary
          if sent
            then pure (Right (recv, cache))
            else do
              errMsg <- Pq.errorMessage connection
              pure (Left (commandToSessionError (ClientError errMsg)))
          where
            recv = do
              putStrLn "executing recv for unprepared statement"
              fmap (first commandToSessionError)
                $ (<*)
                <$> ResultsDecoder.toHandler decoder connection
                <*> ResultsDecoder.toHandler ResultsDecoder.dropRemainders connection

    commandToSessionError =
      QueryError sql (ParamsEncoder.renderReadable encoder params)
