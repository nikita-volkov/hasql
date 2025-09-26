module Core.Contexts.Pipeline where

import Core.Contexts.ParamsEncoder qualified as ParamsEncoder
import Core.Errors
import Core.Location
import Core.Structures.StatementCache qualified as StatementCache
import Hipq.ResultDecoder qualified
import Hipq.Roundtrip qualified
import Platform.Prelude
import Pq qualified

run :: forall a. Pipeline a -> Bool -> Pq.Connection -> StatementCache.StatementCache -> IO (Either Error a, StatementCache.StatementCache)
run (Pipeline totalStatements run) usePreparedStatements connection cache = do
  let (roundtrip, newCache) = run usePreparedStatements cache
  result <- Hipq.Roundtrip.toPipelineIO (Left (InPipeline totalStatements)) roundtrip connection
  case result of
    Left (Hipq.Roundtrip.ClientError _ details) -> do
      Pq.reset connection
      pure (Left (ConnectionError (maybe "Connection error" decodeUtf8Lenient details)), StatementCache.empty)
    Left (Hipq.Roundtrip.ServerError recvError) ->
      pure (Left (fromRecvError recvError), newCache)
    Right a ->
      pure (Right a, newCache)

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
data Pipeline a
  = Pipeline
      -- | Amount of statements in this pipeline.
      Int
      -- | Function that runs the pipeline.
      --
      -- The boolean parameter indicates whether prepared statements should be used when possible.
      --
      -- The function takes the current statement cache and returns a tuple of:
      -- 1. The actual roundtrip action to be executed in the pipeline.
      -- 2. The updated statement cache after executing this part of the pipeline.
      ( Bool ->
        StatementCache.StatementCache ->
        (Hipq.Roundtrip.Roundtrip (Either InPipeline InStatement) a, StatementCache.StatementCache)
      )

instance Functor Pipeline where
  fmap f (Pipeline count run) = Pipeline count \usePreparedStatements cache ->
    let (roundtrip, newCache) = run usePreparedStatements cache
     in (fmap f roundtrip, newCache)

instance Applicative Pipeline where
  pure a =
    Pipeline 0 (\_ cache -> (pure a, cache))

  Pipeline lCount lRun <*> Pipeline rCount rRun =
    Pipeline (lCount + rCount) \usePreparedStatements cache ->
      let (lRoundtrip, cache1) = lRun usePreparedStatements cache
          (rRoundtrip, cache2) = rRun usePreparedStatements cache1
       in (lRoundtrip <*> rRoundtrip, cache2)

-- |
-- Execute a statement in pipelining mode.
statement :: ByteString -> ParamsEncoder.ParamsEncoder params -> Hipq.ResultDecoder.ResultDecoder result -> Bool -> params -> Pipeline result
statement sql encoder decoder preparable params =
  Pipeline 1 run
  where
    run usePreparedStatements =
      if prepare
        then runPrepared
        else runUnprepared
      where
        (oidList, valueAndFormatList) =
          ParamsEncoder.compilePreparedStatementData encoder params

        prepare =
          usePreparedStatements && preparable

        context =
          Right
            ( InStatement
                (InPipeline 1)
                0
                sql
                (ParamsEncoder.renderReadable encoder params)
                prepare
            )

        runPrepared cache =
          (roundtrip, newCache)
          where
            (isNew, remoteKey, newCache) =
              case StatementCache.lookup localKey cache of
                Just remoteKey -> (False, remoteKey, cache)
                Nothing ->
                  let (remoteKey, newCache) = StatementCache.insert localKey cache
                   in (True, remoteKey, newCache)
              where
                localKey =
                  StatementCache.LocalKey sql oidList
            roundtrip =
              when isNew (Hipq.Roundtrip.prepare context remoteKey sql oidList)
                *> Hipq.Roundtrip.queryPrepared context remoteKey valueAndFormatList Pq.Binary decoder

        runUnprepared cache =
          let roundtrip =
                Hipq.Roundtrip.queryParams context sql (ParamsEncoder.compileUnpreparedStatementData encoder params) Pq.Binary decoder
           in (roundtrip, cache)
