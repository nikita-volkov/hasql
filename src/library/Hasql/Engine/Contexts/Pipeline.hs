module Hasql.Engine.Contexts.Pipeline
  ( Pipeline,
    run,
    statement,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Hasql.Codecs.Encoders.Params qualified as Params
import Hasql.Codecs.RequestingOid qualified as RequestingOid
import Hasql.Comms.Roundtrip qualified as Comms.Roundtrip
import Hasql.Engine.Decoders.Result qualified as Decoders.Result
import Hasql.Engine.Errors qualified as Errors
import Hasql.Engine.PqProcedures.SelectTypeInfo qualified as PqProcedures.SelectTypeInfo
import Hasql.Engine.Structures.OidCache qualified as OidCache
import Hasql.Engine.Structures.StatementCache qualified as StatementCache
import Hasql.Platform.Prelude
import Hasql.Pq qualified as Pq

run ::
  Pipeline a ->
  Bool ->
  Pq.Connection ->
  OidCache.OidCache ->
  StatementCache.StatementCache ->
  IO
    ( Either Errors.SessionError a,
      OidCache.OidCache,
      StatementCache.StatementCache
    )
run (Pipeline totalStatements unknownTypes run) usePreparedStatements connection oidCache statementCache = do
  let missingTypes = OidCache.selectUnknownNames unknownTypes oidCache
  oidCacheUpdates <- PqProcedures.SelectTypeInfo.run connection (PqProcedures.SelectTypeInfo.SelectTypeInfo missingTypes)
  case oidCacheUpdates of
    Left err -> pure (Left err, oidCache, statementCache)
    Right oidCacheUpdates -> do
      -- Validate that all requested types were found
      let foundTypes = HashMap.keysSet oidCacheUpdates
          notFoundTypes = HashSet.difference missingTypes foundTypes
      if not (HashSet.null notFoundTypes)
        then pure (Left (Errors.MissingTypesSessionError notFoundTypes), oidCache, statementCache)
        else do
          let newOidCache = oidCache <> OidCache.fromHashMap oidCacheUpdates

          let (roundtrip, newStatementCache) = run 0 usePreparedStatements (OidCache.toHashMap newOidCache) statementCache

          result <-
            let adaptedRoundtrip = first adaptContext roundtrip
                  where
                    adaptContext :: Context -> Maybe (Int, Int, ByteString, [Text], Bool)
                    adaptContext (Context index sql params prepared) =
                      Just (totalStatements, index, sql, params, prepared)
             in Comms.Roundtrip.toPipelineIO adaptedRoundtrip Nothing connection
                  & fmap
                    ( first
                        ( \case
                            Comms.Roundtrip.ClientError _context details ->
                              Errors.ConnectionSessionError (maybe "" decodeUtf8Lenient details)
                            Comms.Roundtrip.ServerError recvError ->
                              Errors.fromRecvError recvError
                        )
                    )

          pure (result, newOidCache, newStatementCache)

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
      -- | Names of types that are used in this pipeline.
      --
      -- They will be used to pre-resolve type OIDs before running the pipeline providing them in OidCache.
      -- It can be assumed in the execution function that these types are always present in the cache.
      -- To achieve that property we will be validating the presence of all requested types in the database or failing before running the pipeline.
      -- In the execution function we will be defaulting to 'Pq.Oid 0' for unknown types as a fallback in case of bugs.
      (HashSet (Maybe Text, Text))
      -- | Function that runs the pipeline.
      --
      -- The integer parameter indicates the current offset of the statement in the pipeline (0-based).
      --
      -- The boolean parameter indicates whether preparable statements should be prepared.
      --
      -- OidCache is provided in which the names of types used in this pipeline are already resolved.
      --
      -- The function takes the current statement cache and returns a tuple of:
      -- 1. The actual roundtrip action to be executed in the pipeline.
      -- 2. The updated statement cache after executing this part of the pipeline.
      ( Int ->
        Bool ->
        HashMap (Maybe Text, Text) (Word32, Word32) ->
        StatementCache.StatementCache ->
        (Comms.Roundtrip.Roundtrip Context a, StatementCache.StatementCache)
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
  fmap f (Pipeline count unknownTypes run) = Pipeline count unknownTypes \offset usePreparedStatements oidCache cache ->
    let (roundtrip, newStatementCache) = run offset usePreparedStatements oidCache cache
     in (fmap f roundtrip, newStatementCache)

instance Applicative Pipeline where
  pure a =
    Pipeline 0 mempty (\_ _ _ cache -> (pure a, cache))

  Pipeline lCount leftUnknownTypes lRun <*> Pipeline rCount rightUnknownTypes rRun =
    let unknownTypes = leftUnknownTypes <> rightUnknownTypes
     in Pipeline (lCount + rCount) unknownTypes \offset usePreparedStatements oidCache statementCache ->
          let (lRoundtrip, statementCache1) = lRun offset usePreparedStatements oidCache statementCache
              offset1 = offset + lCount
              (rRoundtrip, statementCache2) = rRun offset1 usePreparedStatements oidCache statementCache1
           in (lRoundtrip <*> rRoundtrip, statementCache2)

-- * Construction

-- |
-- Execute a statement in pipelining mode.
statement ::
  ByteString ->
  Params.Params params ->
  Decoders.Result.Result result ->
  Bool ->
  params ->
  Pipeline result
statement sql encoder (Decoders.Result.unwrap -> decoder) preparable params =
  Pipeline 1 unknownTypes run
  where
    unknownTypes =
      Params.toUnknownTypes encoder
        <> RequestingOid.toUnknownTypes decoder
    run offset usePreparedStatements oidCache =
      if prepare
        then runPrepared
        else runUnprepared
      where
        (oidList, valueList) =
          Params.compilePreparedStatementData encoder oidCache params

        pqOidList =
          fmap (Pq.Oid . fromIntegral) oidList

        prepare =
          usePreparedStatements && preparable

        context =
          Context
            offset
            sql
            (Params.renderReadable encoder params)
            prepare

        runPrepared statementCache =
          (roundtrip, newStatementCache)
          where
            (isNew, remoteKey, newStatementCache) =
              case StatementCache.lookup sql pqOidList statementCache of
                Just remoteKey -> (False, remoteKey, statementCache)
                Nothing ->
                  let (remoteKey, newStatementCache) = StatementCache.insert sql pqOidList statementCache
                   in (True, remoteKey, newStatementCache)

            roundtrip =
              when isNew (Comms.Roundtrip.prepare context remoteKey sql pqOidList)
                *> Comms.Roundtrip.queryPrepared context remoteKey encodedParams Pq.Binary decoder'
              where
                encodedParams =
                  valueList
                    & fmap (fmap (,Pq.Binary))

        runUnprepared statementCache =
          (roundtrip, statementCache)
          where
            roundtrip =
              Comms.Roundtrip.queryParams context sql encodedParams Pq.Binary decoder'
              where
                encodedParams =
                  params
                    & Params.compileUnpreparedStatementData encoder oidCache
                    & fmap (fmap (\(oid, bytes) -> (Pq.Oid (fromIntegral oid), bytes, Pq.Binary)))

        decoder' =
          RequestingOid.toBase decoder oidCache
