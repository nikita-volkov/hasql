module Core.Contexts.Pipeline where

import Core.Contexts.ParamsEncoder qualified as ParamsEncoder
import Core.Errors
import Core.Structures.StatementCache qualified as StatementCache
import Hipq.Recv qualified
import Hipq.ResultDecoder qualified
import Hipq.ResultRowDecoder qualified
import Hipq.Roundtrip qualified
import Platform.Prelude
import Pq qualified

run :: forall a. Pipeline a -> Bool -> Pq.Connection -> StatementCache.StatementCache -> IO (Either SessionError a, StatementCache.StatementCache)
run (Pipeline run) usePreparedStatements connection cache = do
  let (roundtrip, newCache) = run usePreparedStatements cache
  result <- Hipq.Roundtrip.toPipelineIO roundtrip connection
  case result of
    Left (Hipq.Roundtrip.ClientError details) -> do
      Pq.reset connection
      pure (Left (PipelineError (ClientError details)), StatementCache.empty)
    Left (Hipq.Roundtrip.RecvError recvError) ->
      pure (Left (PipelineError (ResultError (adaptRecvError recvError))), newCache)
    Right a ->
      pure (Right a, newCache)
  where
    adaptRecvError :: Hipq.Recv.Error -> ResultError
    adaptRecvError = \case
      Hipq.Recv.ResultError _offset resultError -> adaptResultError resultError
      Hipq.Recv.NoResultsError details -> error ("TODO: NoResultsError: " <> show details)
      Hipq.Recv.TooManyResultsError count -> error ("TODO: TooManyResultsError: " <> show count)

    adaptResultError :: Hipq.ResultDecoder.Error -> ResultError
    adaptResultError = \case
      Hipq.ResultDecoder.ServerError code message detail hint position -> ServerError code message detail hint position
      Hipq.ResultDecoder.UnexpectedResult message -> UnexpectedResult message
      Hipq.ResultDecoder.UnexpectedAmountOfRows actual -> UnexpectedAmountOfRows actual
      Hipq.ResultDecoder.UnexpectedAmountOfColumns expected actual -> UnexpectedAmountOfColumns expected actual
      Hipq.ResultDecoder.DecoderTypeMismatch column expected actual -> DecoderTypeMismatch column expected actual
      Hipq.ResultDecoder.RowError rowIndex rowError -> adaptRowError rowIndex rowError

    adaptRowError :: Int -> Hipq.ResultRowDecoder.Error -> ResultError
    adaptRowError rowIndex = \case
      Hipq.ResultRowDecoder.CellError column cellError -> CellError rowIndex column case cellError of
        Hipq.ResultRowDecoder.DecodingCellError message -> ValueError message
        Hipq.ResultRowDecoder.UnexpectedNullCellError -> UnexpectedNull

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
        StatementCache.StatementCache ->
        (Hipq.Roundtrip.Roundtrip a, StatementCache.StatementCache)
      )
  deriving stock (Functor)

instance Applicative Pipeline where
  pure a =
    Pipeline (\_ cache -> (pure a, cache))

  Pipeline lSend <*> Pipeline rSend =
    Pipeline \usePreparedStatements cache ->
      let (lRecv, cache1) = lSend usePreparedStatements cache
          (rRecv, cache2) = rSend usePreparedStatements cache1
       in (lRecv <*> rRecv, cache2)

-- |
-- Execute a statement in pipelining mode.
statement :: ByteString -> ParamsEncoder.ParamsEncoder params -> Hipq.ResultDecoder.ResultDecoder result -> Bool -> params -> Pipeline result
statement sql encoder decoder preparable params =
  Pipeline run
  where
    run usePreparedStatements =
      if usePreparedStatements && preparable
        then runPrepared
        else runUnprepared
      where
        (oidList, valueAndFormatList) =
          ParamsEncoder.compilePreparedStatementData encoder params

        runPrepared cache =
          (roundtrip, newCache)
          where
            (isNew, remoteKey, newCache) = case StatementCache.lookup localKey cache of
              Just remoteKey ->
                (False, remoteKey, cache)
              Nothing ->
                let (remoteKey, newCache) = StatementCache.insert localKey cache
                 in (True, remoteKey, newCache)
              where
                localKey =
                  StatementCache.LocalKey sql oidList

            roundtrip = do
              when isNew (Hipq.Roundtrip.prepare remoteKey sql oidList)
              res <- Hipq.Roundtrip.queryPrepared remoteKey valueAndFormatList Pq.Binary decoder
              pure res

        runUnprepared cache =
          (roundtrip, cache)
          where
            roundtrip =
              Hipq.Roundtrip.queryParams sql (ParamsEncoder.compileUnpreparedStatementData encoder params) Pq.Binary decoder
