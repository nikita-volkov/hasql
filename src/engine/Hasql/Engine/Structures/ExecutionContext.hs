-- |
-- What the driver needs to know about an operation it has sent to the server
-- once that operation comes back as a failure.
module Hasql.Engine.Structures.ExecutionContext
  ( ExecutionContext (..),
    toStatementLocation,
    recoverStatementCache,
  )
where

import Hasql.Comms.Roundtrip qualified as Comms.Roundtrip
import Hasql.Engine.Errors qualified as Errors
import Hasql.Engine.Structures.StatementCache qualified as StatementCache
import Hasql.Platform.Prelude

-- |
-- Context attached to every operation of a roundtrip.
--
-- It serves two purposes. It attributes a failure to the statement that caused
-- it, for error reporting. And it carries the statement cache as it stood
-- before the operation, so that a failure can be recovered from without the
-- client's idea of what is prepared drifting away from the server's: message
-- ordering guarantees that everything preceding the failing operation did
-- execute, and nothing following it did.
data ExecutionContext = ExecutionContext
  { -- | 0-based offset of the statement within its pipeline. Zero when
    -- executed alone.
    statementIndex :: Int,
    -- | SQL template of the statement.
    sql :: ByteString,
    -- | Parameters in human-readable form.
    params :: [Text],
    -- | Whether this execution used a prepared statement.
    isPrepared :: Bool,
    -- | Cache identity of the statement, present when it was executing as a
    -- prepared one. Lets the driver invalidate exactly this entry when the
    -- server reports its plan as stale.
    localKey :: Maybe StatementCache.LocalKey,
    -- | Statement cache that is known to agree with the server if this
    -- operation is the one that failed.
    statementCache :: StatementCache.StatementCache
  }
  deriving stock (Show, Eq)

-- |
-- Render the context into the statement location tuple that error
-- construction expects, given the total amount of statements in the pipeline.
toStatementLocation :: Int -> ExecutionContext -> (Int, Int, ByteString, [Text], Bool)
toStatementLocation totalStatements ExecutionContext {..} =
  (totalStatements, statementIndex, sql, params, isPrepared)

-- |
-- The statement cache to carry forward after a roundtrip has failed.
--
-- Automatic preparation makes two server conditions everyone's problem, where
-- previously an explicitly unpreparable statement was the escape hatch, so
-- both are handled here rather than surfaced to the user.
recoverStatementCache ::
  -- | Whether the failed roundtrip was sent in pipeline mode.
  Bool ->
  -- | Cache to fall back to when the failure carries no context of its own.
  StatementCache.StatementCache ->
  Comms.Roundtrip.Error (Maybe ExecutionContext) ->
  StatementCache.StatementCache
recoverStatementCache isPipelined fallback failure =
  case failure of
    -- A send-side failure can mean the batch never reached the server, so the
    -- recovered snapshot may claim deallocations that never happened, which
    -- would orphan those names for the life of the connection. Only a
    -- DEALLOCATE ALL restores exact agreement, and in serial mode there is
    -- nothing to disagree about in the first place.
    Comms.Roundtrip.ClientError _ _
      | isPipelined -> StatementCache.markDesynced recovered
      | otherwise -> recovered
    Comms.Roundtrip.ServerError _ ->
      case Errors.toSqlState failure of
        -- "cached plan must not change result type". Relation-specific, so
        -- other statements recover independently instead of the whole pool
        -- suffering a re-PARSE storm on every migration.
        Just "0A000" ->
          case extract failure >>= localKey of
            Just key -> StatementCache.evict key recovered
            Nothing -> recovered
        -- "prepared statement does not exist". Almost always means the whole
        -- server-side set is gone — a proxy handing over a different backend,
        -- or the user's own DISCARD ALL. Evicting one entry at a time would
        -- mean eating one spurious error per cached statement.
        Just "26000" -> StatementCache.flush recovered
        _ -> recovered
  where
    recovered = maybe fallback statementCache (extract failure)
