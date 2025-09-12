module Core.Contexts.Pipeline where

import Core.Contexts.ParamsEncoder qualified as ParamsEncoder
import Core.Contexts.ResultDecoder qualified as ResultDecoder
import Core.Contexts.ResultsDecoder qualified as ResultsDecoder
import Core.Errors
import Core.Structures.StatementCache qualified as StatementCache
import Platform.Prelude
import Pq qualified

run :: forall a. Pipeline a -> Bool -> Pq.Connection -> Bool -> StatementCache.StatementCache -> IO (Either SessionError (a, StatementCache.StatementCache))
run (Pipeline sendQueriesInIO) usePreparedStatements connection integerDatetimes cache = do
  runExceptT do
    enterPipelineMode
    (recvQueries, newCache) <- sendQueries
    pipelineSync
    result <- finallyE recvQueries do
      recvPipelineSync
      exitPipelineMode
    pure (result, newCache)
  where
    enterPipelineMode :: ExceptT SessionError IO ()
    enterPipelineMode =
      runCommand $ Pq.enterPipelineMode connection

    exitPipelineMode :: ExceptT SessionError IO ()
    exitPipelineMode =
      runCommand $ Pq.exitPipelineMode connection

    sendQueries :: ExceptT SessionError IO (ExceptT SessionError IO a, StatementCache.StatementCache)
    sendQueries =
      fmap (\(ioResult, newCache) -> (ExceptT ioResult, newCache)) $ ExceptT $ sendQueriesInIO usePreparedStatements connection integerDatetimes cache

    pipelineSync :: ExceptT SessionError IO ()
    pipelineSync =
      runCommand $ Pq.pipelineSync connection

    recvPipelineSync :: ExceptT SessionError IO ()
    recvPipelineSync =
      runResultsDecoder
        $ ResultsDecoder.single ResultDecoder.pipelineSync

    runResultsDecoder :: forall a. ResultsDecoder.ResultsDecoder a -> ExceptT SessionError IO a
    runResultsDecoder decoder =
      ExceptT
        $ fmap (first PipelineError)
        $ ResultsDecoder.toHandler decoder connection integerDatetimes

    runCommand :: IO Bool -> ExceptT SessionError IO ()
    runCommand action =
      lift action >>= \case
        True -> pure ()
        False -> ExceptT (Left . PipelineError . ClientError <$> Pq.errorMessage connection)

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
        Bool ->
        StatementCache.StatementCache ->
        IO (Either SessionError (IO (Either SessionError a), StatementCache.StatementCache))
      )
  deriving (Functor)

instance Applicative Pipeline where
  pure a =
    Pipeline (\_ _ _ cache -> pure (Right (pure (Right a), cache)))

  Pipeline lSend <*> Pipeline rSend =
    Pipeline \usePreparedStatements conn integerDatetimes cache ->
      lSend usePreparedStatements conn integerDatetimes cache >>= \case
        Left sendErr ->
          pure (Left sendErr)
        Right (lRecv, cache1) ->
          rSend usePreparedStatements conn integerDatetimes cache1 <&> \case
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
    run usePreparedStatements connection integerDatetimes cache =
      if usePreparedStatements && preparable
        then runPrepared
        else runUnprepared
      where
        runPrepared = runExceptT do
          (key, keyRecv, newCache) <- ExceptT resolvePreparedStatementKey
          queryRecv <- ExceptT (sendQuery key)
          pure (keyRecv *> queryRecv, newCache)
          where
            (oidList, valueAndFormatList) =
              ParamsEncoder.compilePreparedStatementData encoder integerDatetimes params

            resolvePreparedStatementKey =
              case StatementCache.lookup localKey cache of
                Just remoteKey -> pure (Right (remoteKey, pure (Right ()), cache))
                Nothing -> do
                  let (remoteKey, newCache) = StatementCache.insert localKey cache
                  sent <- Pq.sendPrepare connection remoteKey sql (mfilter (not . null) (Just oidList))
                  if sent
                    then pure (Right (remoteKey, recv, newCache))
                    else do
                      errMsg <- Pq.errorMessage connection
                      pure (Left (commandToSessionError (ClientError errMsg)))
                  where
                    recv =
                      fmap (first commandToSessionError)
                        $ (<*)
                        <$> ResultsDecoder.toHandler (ResultsDecoder.single ResultDecoder.noResult) connection integerDatetimes
                        <*> ResultsDecoder.toHandler ResultsDecoder.dropRemainders connection integerDatetimes
              where
                localKey =
                  StatementCache.LocalKey sql oidList

            sendQuery key =
              Pq.sendQueryPrepared connection key valueAndFormatList Pq.Binary >>= \case
                False -> Left . commandToSessionError . ClientError <$> Pq.errorMessage connection
                True -> pure (Right recv)
              where
                recv =
                  fmap (first commandToSessionError)
                    $ (<*)
                    <$> ResultsDecoder.toHandler decoder connection integerDatetimes
                    <*> ResultsDecoder.toHandler ResultsDecoder.dropRemainders connection integerDatetimes

        runUnprepared = do
          sent <- Pq.sendQueryParams connection sql (ParamsEncoder.compileUnpreparedStatementData encoder integerDatetimes params) Pq.Binary
          if sent
            then pure (Right (recv, cache))
            else do
              errMsg <- Pq.errorMessage connection
              pure (Left (commandToSessionError (ClientError errMsg)))
          where
            recv =
              fmap (first commandToSessionError)
                $ (<*)
                <$> ResultsDecoder.toHandler decoder connection integerDatetimes
                <*> ResultsDecoder.toHandler ResultsDecoder.dropRemainders connection integerDatetimes

    commandToSessionError =
      QueryError sql (ParamsEncoder.renderReadable encoder params)
