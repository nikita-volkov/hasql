-- |
-- Connection-scoped registry of server-side prepared statements.
--
-- The driver decides on its own which statements are worth preparing, from
-- observed usage. Two policies make that safe:
--
-- * /Admission/. A statement is only prepared once it has been executed
--   a configured number of times. Without this a one-shot dynamically
--   generated statement would still pay a @PARSE@, occupy a slot and evict a
--   hot entry.
--
-- * /Eviction/. The admitted set is bounded, dropping the least recently used
--   entry to make room. The caller is told which entry was dropped so that it
--   can issue the matching @DEALLOCATE@, keeping the server-side set bounded
--   too.
--
-- The structure is purely functional because it is threaded through the
-- 'Applicative' composition of a pipeline, where no mutable state is available.
module Hasql.Engine.Structures.StatementCache
  ( -- * Cache
    StatementCache,
    empty,

    -- * Keys
    LocalKey,
    localKey,
    RemoteKey,

    -- * Execution decisions
    Decision (..),
    decide,

    -- * Invalidation
    evict,
    withdraw,
    flush,
    reset,

    -- * Desync
    markDesynced,
    isDesynced,

    -- * Stats
    StatementCacheStats (..),
    toStats,
  )
where

import Data.HashPSQ qualified as HashPsq
import Data.IntPSQ qualified as IntPsq
import Hasql.Platform.Prelude hiding (empty, insert, lookup, reset)

-- |
-- Client-side identity of a statement: its SQL together with the OIDs its
-- parameters resolved to.
--
-- The OIDs are part of the identity because the same SQL prepared with
-- different parameter types is a different statement on the server.
data LocalKey
  = LocalKey ByteString [Word32]
  deriving stock (Show, Eq, Ord)

instance Hashable LocalKey where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt (LocalKey template oids) =
    hashWithSalt (hashWithSalt salt template) oids

-- |
-- Construct the identity of a statement from its SQL and resolved parameter OIDs.
{-# INLINEABLE localKey #-}
localKey :: ByteString -> [Word32] -> LocalKey
localKey = LocalKey

-- |
-- Name a statement is prepared under on the server.
type RemoteKey = ByteString

-- |
-- Monotonic counter serving as the LRU priority. The smallest value is the
-- least recently used entry.
type Tick = Word64

-- |
-- The state of the cache on a single connection.
data StatementCache = StatementCache
  { -- | Statements currently prepared on the server, bounded by 'capacity'.
    prepared :: HashPsq.HashPSQ LocalKey Tick RemoteKey,
    -- | Execution counts of statements not admitted yet, keyed by the hash of
    -- their 'LocalKey' and bounded by 'capacity'.
    --
    -- Keying by hash means the SQL of dynamically generated statements is not
    -- retained and an entry costs a handful of words regardless of statement
    -- length. A hash collision merely admits a statement early, which is
    -- harmless.
    pending :: IntPsq.IntPSQ Tick Int,
    -- | Maximum amount of statements prepared at a time. Zero disables
    -- preparation altogether.
    capacity :: Int,
    -- | Execution count at which a statement gets admitted. One means
    -- preparing on first use.
    threshold :: Int,
    -- | Source of LRU priorities.
    tick :: Tick,
    -- | Source of remote keys.
    --
    -- Monotonic for the life of the connection. Resetting it is only sound
    -- together with a @DEALLOCATE ALL@, since otherwise a surviving
    -- server-side statement would collide with a reused name as @42P05@.
    counter :: Word,
    -- | Whether the cache may disagree with the server about what is prepared,
    -- requiring a @DEALLOCATE ALL@ before the connection is used again.
    desynced :: Bool,
    admissions :: Int,
    hits :: Int,
    misses :: Int,
    evictions :: Int
  }
  deriving stock (Eq)

instance Show StatementCache where
  showsPrec d StatementCache {..} =
    showParen (d > 10)
      $ showString "StatementCache "
      . showsPrec 11 (HashPsq.toList prepared)
      . showChar ' '
      . showsPrec 11 (IntPsq.toList pending)
      . showChar ' '
      . showsPrec 11 (capacity, threshold, tick, counter, desynced)
      . showChar ' '
      . showsPrec 11 (admissions, hits, misses, evictions)

-- |
-- Cache holding nothing, configured with the maximum amount of statements to
-- keep prepared and the execution count at which a statement gets admitted.
--
-- Both settings are clamped into their valid ranges: a negative capacity
-- becomes zero (preparation disabled) and a threshold below one becomes one
-- (prepare on first use). Only the capacity can disable preparation.
{-# INLINEABLE empty #-}
empty :: Int -> Int -> StatementCache
empty capacity threshold =
  StatementCache
    { prepared = HashPsq.empty,
      pending = IntPsq.empty,
      capacity = max 0 capacity,
      threshold = max 1 threshold,
      tick = 0,
      counter = 0,
      desynced = False,
      admissions = 0,
      hits = 0,
      misses = 0,
      evictions = 0
    }

-- |
-- What the driver should do to execute a statement.
data Decision
  = -- | The statement is already prepared under the given remote key.
    HitDecision RemoteKey
  | -- | The statement is not prepared and is not worth preparing yet.
    -- Execute it with inline parameter OIDs.
    MissDecision
  | -- | The statement has earned preparation. Prepare it under the given
    -- remote key, first deallocating the reported victim if the cache had to
    -- make room.
    AdmitDecision RemoteKey (Maybe RemoteKey)
  deriving stock (Show, Eq)

-- |
-- Decide how to execute a statement, registering the execution.
--
-- This is the only place where usage is observed, so it must be called exactly
-- once per statement execution, and the resulting cache committed once the
-- decision has been carried out.
{-# INLINEABLE decide #-}
decide :: LocalKey -> StatementCache -> (Decision, StatementCache)
decide key cache@StatementCache {..} =
  case HashPsq.lookup key prepared of
    Just (_, remoteKey) ->
      ( HitDecision remoteKey,
        cache
          { prepared = HashPsq.insert key tick remoteKey prepared,
            tick = tick + 1,
            hits = hits + 1
          }
      )
    Nothing
      | capacity <= 0 ->
          (MissDecision, cache {misses = misses + 1})
      | count < threshold ->
          ( MissDecision,
            cache
              { pending = bumpPending,
                tick = tick + 1,
                misses = misses + 1
              }
          )
      | otherwise ->
          ( AdmitDecision remoteKey victim,
            cache
              { prepared = HashPsq.insert key tick remoteKey preparedWithRoom,
                pending = IntPsq.delete hashKey pending,
                tick = tick + 1,
                counter = counter + 1,
                admissions = admissions + 1,
                misses = misses + 1,
                evictions = evictions + maybe 0 (const 1) victim
              }
          )
  where
    hashKey = hash key

    count = maybe 0 snd (IntPsq.lookup hashKey pending) + 1

    -- Pending entries are bounded on their own so that a stream of unique SQL
    -- cannot displace the hot prepared set.
    bumpPending =
      let bumped = snd (IntPsq.insertView hashKey tick count pending)
       in if IntPsq.size bumped > capacity
            then maybe bumped (\(_, _, _, rest) -> rest) (IntPsq.minView bumped)
            else bumped

    remoteKey = fromString (show counter)

    (victim, preparedWithRoom) =
      if HashPsq.size prepared >= capacity
        then case HashPsq.minView prepared of
          Just (_, _, victimKey, rest) -> (Just victimKey, rest)
          Nothing -> (Nothing, prepared)
        else (Nothing, prepared)

-- |
-- Drop a single statement, for when the server rejected its cached plan.
--
-- Other statements keep their entries, so a migration does not cost a
-- re-@PARSE@ storm across every statement on the connection.
{-# INLINEABLE evict #-}
evict :: LocalKey -> StatementCache -> StatementCache
evict key cache@StatementCache {..} =
  case HashPsq.deleteView key prepared of
    Nothing -> cache
    Just (_, _, rest) -> cache {prepared = rest, evictions = evictions + 1}

-- |
-- Drop an entry that never reached the server, undoing an admission whose
-- @PARSE@ failed.
--
-- Unlike 'evict' this is not an invalidation of anything the server holds, so
-- it leaves the stats alone.
{-# INLINEABLE withdraw #-}
withdraw :: LocalKey -> StatementCache -> StatementCache
withdraw key cache@StatementCache {..} =
  cache {prepared = HashPsq.delete key prepared}

-- |
-- Drop every statement without deallocating anything, for when the server has
-- demonstrably lost the whole set.
--
-- The remote key counter is deliberately left alone: any statement that did
-- survive on the server would otherwise collide with a reused name.
{-# INLINEABLE flush #-}
flush :: StatementCache -> StatementCache
flush cache@StatementCache {..} =
  cache
    { prepared = HashPsq.empty,
      evictions = evictions + HashPsq.size prepared
    }

-- |
-- Bring the cache back to its initial state, including the remote key counter.
--
-- Only sound when paired with a @DEALLOCATE ALL@ against the server.
-- Configuration and lifetime stats are preserved.
{-# INLINEABLE reset #-}
reset :: StatementCache -> StatementCache
reset cache@StatementCache {..} =
  cache
    { prepared = HashPsq.empty,
      pending = IntPsq.empty,
      tick = 0,
      counter = 0,
      desynced = False,
      evictions = evictions + HashPsq.size prepared
    }

-- |
-- Record that the cache may no longer agree with the server, clearing it and
-- scheduling a @DEALLOCATE ALL@ for the next use of the connection.
--
-- Needed after a client-side failure of a pipeline, where it is unknown
-- whether the batch reached the server at all, so the deallocations it
-- contained may or may not have happened.
{-# INLINEABLE markDesynced #-}
markDesynced :: StatementCache -> StatementCache
markDesynced cache@StatementCache {..} =
  cache
    { prepared = HashPsq.empty,
      desynced = True,
      evictions = evictions + HashPsq.size prepared
    }

{-# INLINEABLE isDesynced #-}
isDesynced :: StatementCache -> Bool
isDesynced = desynced

-- |
-- Observable state of the cache, for sizing the configuration from evidence.
data StatementCacheStats = StatementCacheStats
  { -- | Amount of statements currently prepared on the server.
    size :: Int,
    -- | Amount of statements that have been prepared over the lifetime of the
    -- connection.
    admissions :: Int,
    -- | Executions that used an already prepared statement.
    hits :: Int,
    -- | Executions that did not. Includes the execution that triggers an
    -- admission, since the statement was not prepared when it was looked up.
    misses :: Int,
    -- | Entries dropped from the cache, whether to make room or because the
    -- server invalidated them.
    evictions :: Int
  }
  deriving stock (Show, Eq)

{-# INLINEABLE toStats #-}
toStats :: StatementCache -> StatementCacheStats
toStats StatementCache {..} =
  StatementCacheStats
    { size = HashPsq.size prepared,
      admissions = admissions,
      hits = hits,
      misses = misses,
      evictions = evictions
    }
