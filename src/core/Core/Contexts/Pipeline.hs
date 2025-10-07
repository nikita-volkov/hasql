module Core.Contexts.Pipeline
  ( Pipeline,
    run,
    statement,
  )
where

import Core.Contexts.ParamsEncoder qualified as ParamsEncoder
import Core.Errors qualified as Errors
import Core.Structures.StatementCache qualified as StatementCache
import Hipq.ResultDecoder qualified
import Hipq.Roundtrip qualified
import Platform.Prelude
import Pq qualified

run :: Pipeline a -> Bool -> Pq.Connection -> StatementCache.StatementCache -> IO (Either Errors.SessionError a, StatementCache.StatementCache)
run (Pipeline totalStatements run) usePreparedStatements connection cache = do
  let (roundtrip, newCache) = run 0 usePreparedStatements cache
      adaptedRoundtrip = first adaptContext roundtrip
  result <- Hipq.Roundtrip.toPipelineIO Nothing adaptedRoundtrip connection
  case result of
    Left (Hipq.Roundtrip.ClientError _context details) -> do
      Pq.reset connection
      pure (Left (Errors.ConnectionSessionError (maybe "Connection error" decodeUtf8Lenient details)), StatementCache.empty)
    Left (Hipq.Roundtrip.ServerError recvError) ->
      pure (Left (Errors.fromRecvError recvError), newCache)
    Right a ->
      pure (Right a, newCache)
  where
    adaptContext :: Context -> Maybe (Int, Int, ByteString, [Text], Bool)
    adaptContext (Context index sql params prepared) =
      Just (totalStatements, index, sql, params, prepared)

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
      -- The integer parameter indicates the current offset of the statement in the pipeline (0-based).
      --
      -- The boolean parameter indicates whether prepared statements should be used when possible.
      --
      -- The function takes the current statement cache and returns a tuple of:
      -- 1. The actual roundtrip action to be executed in the pipeline.
      -- 2. The updated statement cache after executing this part of the pipeline.
      ( Int ->
        Bool ->
        StatementCache.StatementCache ->
        (Hipq.Roundtrip.Roundtrip Context a, StatementCache.StatementCache)
      )

data Context
  = Context
      -- | Offset of the statement in the pipeline (0-based).
      Int
      -- | SQL.
      ByteString
      -- | Parameters in a human-readable form.
      [Text]
      -- | Whether the statement is prepared.
      Bool
  deriving stock (Show, Eq)

-- * Instances

instance Functor Pipeline where
  fmap f (Pipeline count run) = Pipeline count \offset usePreparedStatements cache ->
    let (roundtrip, newCache) = run offset usePreparedStatements cache
     in (fmap f roundtrip, newCache)

instance Applicative Pipeline where
  pure a =
    Pipeline 0 (\_ _ cache -> (pure a, cache))

  Pipeline lCount lRun <*> Pipeline rCount rRun =
    Pipeline (lCount + rCount) \offset usePreparedStatements cache ->
      let (lRoundtrip, cache1) = lRun offset usePreparedStatements cache
          offset1 = offset + lCount
          (rRoundtrip, cache2) = rRun offset1 usePreparedStatements cache1
       in (lRoundtrip <*> rRoundtrip, cache2)

-- * Construction

-- |
-- Execute a statement in pipelining mode.
statement :: ByteString -> ParamsEncoder.ParamsEncoder params -> Hipq.ResultDecoder.ResultDecoder result -> Bool -> params -> Pipeline result
statement sql encoder decoder preparable params =
  Pipeline 1 run
  where
    run offset usePreparedStatements =
      if prepare
        then runPrepared
        else runUnprepared
      where
        (oidList, valueAndFormatList) =
          ParamsEncoder.compilePreparedStatementData encoder params

        prepare =
          usePreparedStatements && preparable

        context =
          Context
            offset
            sql
            (ParamsEncoder.renderReadable encoder params)
            prepare

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
