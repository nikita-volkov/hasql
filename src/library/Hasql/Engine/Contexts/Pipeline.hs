module Hasql.Engine.Contexts.Pipeline
  ( Pipeline,
    run,
    statement,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Hasql.CodecsVocab qualified as CodecsVocab
import Hasql.CodecsVocab.QualifiedTypeName qualified as CodecsVocab.QualifiedTypeName
import Hasql.Comms.Roundtrip qualified as Comms.Roundtrip
import Hasql.ConnectionState qualified as ConnectionState
import Hasql.ConnectionState.OidCache qualified as OidCache
import Hasql.ConnectionState.StatementCache qualified as StatementCache
import Hasql.Engine.Errors qualified as Errors
import Hasql.Engine.PqProcedures.SelectTypeInfo qualified as PqProcedures.SelectTypeInfo
import Hasql.Engine.Statement qualified as Statement
import Hasql.Platform.Prelude
import Pqi qualified as Pq

run ::
  Pipeline a ->
  ConnectionState.ConnectionState ->
  IO (Either Errors.UseError a, ConnectionState.ConnectionState)
run (Pipeline totalStatements unknownTypes runPipeline) connectionState@ConnectionState.ConnectionState {..} = do
  let missingTypes = OidCache.selectUnknownNames unknownTypes oidCache
  resolvedOidCache <-
    if HashSet.null missingTypes
      then pure (Right oidCache)
      else do
        oidCacheUpdates <-
          PqProcedures.SelectTypeInfo.run connection (PqProcedures.SelectTypeInfo.SelectTypeInfo missingTypes)
        pure $ case oidCacheUpdates of
          Left err -> Left err
          Right oidCacheUpdates ->
            let foundTypes = HashMap.keysSet oidCacheUpdates
                notFoundTypes = HashSet.difference missingTypes foundTypes
             in if not (HashSet.null notFoundTypes)
                  then Left (Errors.SessionUseError (Errors.MissingTypesSessionError (HashSet.map CodecsVocab.QualifiedTypeName.toNameTuple notFoundTypes)))
                  else Right (oidCache <> OidCache.fromHashMap oidCacheUpdates)
  case resolvedOidCache of
    Left err -> pure (Left err, connectionState)
    Right newOidCache -> do
      let (roundtrip, newStatementCache) =
            runPipeline 0 preparedStatements newOidCache statementCache

      -- Statements carry their tag; the pipeline bookend commands below get none.
      executionResult <- Comms.Roundtrip.toPipelineIO (first Just roundtrip) Nothing connection

      let result =
            first
              ( \case
                  Comms.Roundtrip.ClientError _tag connectionLost details ->
                    Errors.fromSendError connectionLost details
                  Comms.Roundtrip.ServerError recvError ->
                    Errors.fromRecvError (fmap (fmap (\(StatementTag index sql params prepared _ _) -> (totalStatements, index, sql, params, prepared))) recvError)
              )
              executionResult
          finalStatementCache =
            case executionResult of
              Right _ -> newStatementCache
              Left executionError ->
                case extract executionError of
                  Nothing -> statementCache
                  Just (StatementTag _ _ _ _ soFarStatementCache collisionStatementCache) ->
                    -- 42P05 can only be reported in response to our own
                    -- Parse, so the collision cache (with this statement's
                    -- own mapping committed) is safe to adopt whenever it
                    -- occurs.
                    if Errors.isPrepareCollision executionError
                      then collisionStatementCache
                      else soFarStatementCache

      pure
        ( result,
          connectionState
            { ConnectionState.oidCache = newOidCache,
              ConnectionState.statementCache = finalStatementCache
            }
        )

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
-- That's why 'Pipeline' does not have the 'Monad' instance.
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
--     for orders $ \order ->
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
      -- In the execution function we will be defaulting to OID 0 for unknown types as a fallback in case of bugs.
      (HashSet CodecsVocab.QualifiedTypeName)
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
      -- 2. The updated statement cache after composing this part of the pipeline.
      --
      -- The resulting cache is optimistic: on failure we recover the last known
      -- committed cache from statement tag carried by roundtrip errors.
      ( Int ->
        Bool ->
        OidCache.OidCache ->
        StatementCache.StatementCache ->
        (Comms.Roundtrip.Roundtrip StatementTag a, StatementCache.StatementCache)
      )

-- | Tag of a statement execution in a pipeline,
-- carried by roundtrip errors to identify the statement and recover the cache.
data StatementTag
  = StatementTag
      -- | Offset of the statement in the pipeline (0-based).
      Int
      -- | SQL.
      ByteString
      -- | Parameters in a human-readable form.
      [Text]
      -- | Whether the statement is prepared.
      Bool
      -- | The cache to recover to on an ordinary failure.
      StatementCache.StatementCache
      -- | The cache to recover to when the failure is a @Parse@ hitting a
      -- 42P05 name collision, i.e., with this statement's own mapping
      -- committed. Since 42P05 can only ever be reported in response to our
      -- own @Parse@, this is only consulted for the tag attached to the
      -- @Parse@ step; elsewhere it's the same as the ordinary recovery
      -- cache.
      StatementCache.StatementCache
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
  Statement.Statement params result ->
  params ->
  Pipeline result
statement stmt params =
  Pipeline 1 (Statement.unknownTypes stmt) run
  where
    sql = Statement.sql stmt
    run offset usePreparedStatements oidCache =
      if prepare
        then runPrepared
        else runUnprepared
      where
        resolve = OidCache.toResolver oidCache

        (oidList, valueAndFormatList) =
          Statement.compilePreparedStatementData stmt resolve params

        prepare =
          usePreparedStatements && Statement.isPrepared stmt

        tag soFarStatementCache collisionStatementCache =
          StatementTag
            offset
            sql
            (Statement.printer stmt params)
            prepare
            soFarStatementCache
            collisionStatementCache

        runPrepared statementCache =
          (roundtrip, newStatementCache)
          where
            (isNew, remoteKey, newStatementCache) =
              case StatementCache.lookup sql oidList statementCache of
                Just remoteKey -> (False, remoteKey, statementCache)
                Nothing ->
                  let (remoteKey, newStatementCache) = StatementCache.insert sql oidList statementCache
                   in (True, remoteKey, newStatementCache)

            roundtrip =
              when
                isNew
                (Comms.Roundtrip.prepare (tag statementCache newStatementCache) remoteKey sql oidList)
                *> Comms.Roundtrip.queryPrepared (tag newStatementCache newStatementCache) remoteKey encodedParams Pq.Binary decoder'
              where
                encodedParams =
                  valueAndFormatList
                    & fmap (fmap (\(bytes, format) -> (bytes, bool Pq.Binary Pq.Text format)))

        runUnprepared statementCache =
          (roundtrip, statementCache)
          where
            roundtrip =
              Comms.Roundtrip.queryParams (tag statementCache statementCache) sql encodedParams Pq.Binary decoder'
              where
                encodedParams =
                  Statement.compileUnpreparedStatementData stmt resolve params
                    & fmap (fmap (\(oid, bytes, format) -> (oid, bytes, bool Pq.Binary Pq.Text format)))

        decoder' =
          Statement.decoder stmt resolve
