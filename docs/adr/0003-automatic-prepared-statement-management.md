# 3. Automatic prepared-statement management via an LRU cache

Date: 2026-07-23

## Status

Accepted — implementation pending. Targets version `1.11`.

Resolves [issue #310](https://github.com/nikita-volkov/hasql/issues/310).

## Context

`Hasql.Statement` currently offers two constructors, `preparable` and
`unpreparable`, forcing every user to make a per-statement judgement they are
badly placed to make: preparation is only worthwhile if the statement is
executed repeatedly on the *same connection*, which depends on runtime traffic
and pool behaviour, not on anything visible at the definition site.

The supporting machinery has matching problems. `Hasql.Engine.Structures.StatementCache`
is an unbounded `HashMap` that never evicts and never issues `DEALLOCATE`, so a
connection executing dynamically generated SQL accumulates server-side prepared
statements without limit. Preparation is enabled or disabled wholesale by the
`no_prepared_statements` connection setting.

Issue #310 proposes replacing the distinction with a connection-level LRU cache,
so that hot statements get prepared and cold ones do not, discovered from actual
usage.

A plain LRU does not deliver that. It admits *every* statement on first use, so
a one-shot dynamic statement still pays a `PARSE`, still occupies a slot, and —
worst of all — still evicts a hot entry. Getting the behaviour the issue
describes requires an **admission** policy in addition to an eviction policy.

## Decision

### 1. Public API

`Hasql.Statement.preparable` and `Hasql.Statement.unpreparable` are **deleted**
outright — no deprecated aliases. `unpreparable`'s semantics change materially
(its statements may now be prepared), so it must fail to compile rather than
warn; deleting `preparable` at the same time keeps the surface honest.

They are replaced by a single constructor:

```haskell
Hasql.Statement.statement ::
  Text -> Encoders.Params params -> Decoders.Result result -> Statement params result
```

`Statement` loses its `isPrepared` field. There is **no per-statement opt-out**
of any form. Users who need to suppress a generic-plan regression use
PostgreSQL's own `plan_cache_mode = force_custom_plan`, which is both the
correct fix and finer-grained than anything hasql could offer.

`Hasql.Connection.Settings.noPreparedStatements` is deleted;
`statementCacheSize 0` replaces it.

`StatementSessionError`'s `Bool` field ("whether the statement was executed as a
prepared one") is **retained unchanged**. Its meaning shifts from a static
declaration to a dynamic fact about that execution, which is strictly more
informative.

New in `Hasql.Session`:

```haskell
statementCacheStats :: Session StatementCacheStats
```

reporting current size, admissions, hits, misses and evictions. It is a
`Session` action rather than a `Connection -> IO` one so that it reads the state
while the connection is already held, instead of blocking on the `MVar` behind a
running session.

### 2. Configuration

Two knobs, delivered through the existing connection-string interception
mechanism in `Hasql.Connection.Settings` (`ConnectionString.interceptParam`), so
that `Settings` stays a pure `ConnectionString` newtype with its `IsString`,
`Monoid` and `Show` instances intact, and so that deployments can retune or
disable preparation from a DSN with no code change.

| Param | Constructor | Default |
|---|---|---|
| `statement_cache_size` | `statementCacheSize :: Int -> Settings` | `1024` |
| `prepare_threshold` | `prepareThreshold :: Int -> Settings` | `2` |

- `statement_cache_size = 0` disables preparation entirely; pending-usage
  counting is skipped as well. Negative values clamp to `0`.
- `prepare_threshold` is the execution count at which a statement is admitted.
  `1` means "prepare on first use" (plain LRU). Values below `1` clamp to `1`.
  Size `0` is the only way to disable preparation — the threshold never disables.

Threshold `2` is deliberately lower than pgJDBC's and Npgsql's `5`: hasql-pool
recycles connections, and the cache is per-connection, so a high threshold risks
short-lived connections never preparing anything. At `2` a hot statement pays
exactly one unprepared execution per connection, while one-shot dynamic SQL is
still never prepared.

### 3. Cache structure

Two per-connection structures in `ConnectionState`, both **purely functional and
persistent** — a hard requirement, since the cache is threaded through
`Pipeline`'s `Applicative` composition. Both are backed by a new `psqueues`
dependency, behind a thin wrapper whose `insert` returns the evicted victim so
the caller can issue its `DEALLOCATE`.

| Structure | Type | Capacity |
|---|---|---|
| Prepared | `HashPSQ LocalKey Tick RemoteKey` | `statement_cache_size` |
| Pending | `IntPSQ Tick Word` (hash of `LocalKey` → execution count) | `statement_cache_size` |

`LocalKey` remains `(sql, resolved param OIDs)`, unchanged.

The two structures are **separate on purpose**: if pending entries competed for
the same slots, a stream of unique SQL would evict the hot prepared set, which
is exactly the pollution the admission gate exists to prevent.

Pending entries are keyed by hash only, so dynamic SQL bytes are not retained
and each entry costs a handful of words regardless of statement length. A hash
collision merely promotes a statement early — harmless.

`Tick` is a monotonic counter used as the LRU priority. The remote-key counter
is likewise monotonic for the life of the connection and is reset **only** when
the reset is paired with a `DEALLOCATE ALL`; otherwise surviving server-side
statements would collide with reused names as `42P05`.

### 4. Execution flow

Per statement execution:

1. **Hit in prepared** → serial `queryPrepared`, exactly as today. The hot path
   is untouched.
2. **Miss, count + 1 < threshold** → serial `queryParams`; bump the pending
   counter. Unchanged from today's unprepared path.
3. **Miss, count + 1 ≥ threshold** → admit. Allocate a remote key, evict the LRU
   victim if at capacity, and issue `DEALLOCATE`(victim) + `PARSE` +
   `BIND`/`EXECUTE` as a **single roundtrip** via `Comms.Roundtrip.toPipelineIO`.
   Drop the pending entry.

Only case 3 uses pipeline mode. The serial path is *not* wholesale converted:
converting it would change every user's hot path for no benefit on a cache hit.
As a side effect, case 3 costs one roundtrip where today's serial first-prepare
costs two ([`Session.hs:144`](../../src/library/Hasql/Engine/Contexts/Session.hs#L144)
notes libpq's prohibition on back-to-back `PARSE`+`EXECUTE` outside pipeline
mode) — automatic preparation makes that path universal, so the improvement
matters.

Inside `Pipeline` the same decisions apply, with the operations appended in
order into the pipeline already being built. Deallocation there is free.
Message ordering makes mid-pipeline eviction correct without special handling:
a statement evicted at position *n* and used again at position *m > n* simply
misses and is re-parsed under a new key.

Deallocation is issued as `DEALLOCATE "<key>"` through `sendQueryParams` with
zero parameters. This uses the extended query protocol, so it is legal inside
pipeline mode and portable back to PostgreSQL 9. `PQsendClosePrepared` is not an
option: `postgresql-libpq` provides no binding, and the underlying libpq API
requires version 17.

### 5. Stale-statement recovery

Automatic preparation makes two SQLSTATEs everyone's problem, where previously
`unpreparable` was the escape hatch. Both are raised *before* the statement does
anything, so a retry is semantically safe — but any error inside an open
transaction block poisons it (`25P02`), making a retry there guaranteed to fail.
`PQtransactionStatus` distinguishes the cases.

**`0A000` — cached plan must not change result type** (see
[`notes/specs/cached-plan-result-type-error.md`](../../notes/specs/cached-plan-result-type-error.md)).
Relation-specific. Evict **that entry only**; other statements recover
independently rather than suffering a re-`PARSE` storm across the pool on every
migration.

**`26000` — prepared statement does not exist**. Almost always means the whole
server-side set is gone: pgbouncer handing over a different backend, or the
user's own `DISCARD ALL`/`DEALLOCATE ALL`. **Flush the entire prepared map**
client-side — with *no* `DEALLOCATE` (the entries are presumed already gone) and
*without* resetting the remote-key counter (any survivors would collide as
`42P05`). Per-entry eviction here would mean eating one spurious error per
cached statement, up to 1024 of them.

In both cases, additionally **retry once** when the failure occurred on a serial
execution *and* `PQtransactionStatus` is `TransIdle`. The retry runs through the
**unprepared** path (`queryParams` with inline OIDs): that is structurally
immune to `0A000`, so the single retry is far more likely to succeed, and no
`PARSE` is spent on a statement whose plan just proved unstable. The statement
re-enters the cache through the normal threshold path on its next execution.

Pipelines **never** auto-retry: statements preceding the failure have already
executed, and re-running the batch would double their effects.

### 6. Client/server desync

[`Pipeline.hs:67-75`](../../src/library/Hasql/Engine/Contexts/Pipeline.hs#L67-L75)
reconstructs the cache from the snapshot carried by the failing statement's
`Context`. That is sound for *server* errors, where message ordering guarantees
that deallocations preceding the failure really executed.

It is not sound for a **`ClientError`**: a send-side failure can mean the batch
never reached the server, so the recovered snapshot claims a statement was
deallocated while the server still holds it, orphaning that name for the life of
the connection. A returned `ClientError` does not trigger
`cleanUpAfterInterruption` — only a thrown exception does.

Therefore: any client-side pipeline failure marks the connection's cache as
**desynced**. The cache is cleared, and `DEALLOCATE ALL` is issued at the start
of the next `use`, restoring exact client/server agreement and making a counter
reset safe at that point. This costs one roundtrip on an already-exceptional
path where the connection is likely failing anyway.

The existing `cleanUpAfterInterruption` path (which already issues
`DEALLOCATE ALL`) is unchanged.

### 7. Sequencing

Ships **first, on `master`, as `1.11`** — ahead of the pqi migration and the
`hasql-core` repackaging. This change is semantic and concentrated in
`StatementCache`, `Session` and `Pipeline`; the pqi migration's changes to those
same modules are mechanical type substitutions, so rebasing pqi onto this is far
cheaper than the reverse.

### 8. Testing

- **Pure**, in `engine-tests`: admission exactly at the threshold, LRU eviction
  order, victim reporting from `insert`, pending-structure bounding, flush
  without counter reset, `size = 0` behaviour, config clamping.
- **Integration**, in `library-tests` against PostgreSQL 9 and 18, verified
  through `pg_prepared_statements`: preparation only on the *N*th use, eviction
  actually deallocating, server-side count never exceeding the configured size,
  `size = 0` preparing nothing, mid-pipeline eviction, and the stats accessor.
- **Error recovery** needs no fake server: `0A000` is provokable with
  `ALTER TABLE ... ADD COLUMN` against a `select *` statement, and `26000` with
  `DEALLOCATE ALL` through `Session.script`. Both are tested inside and outside
  an explicit transaction to cover the retry condition.

The `Requirements`-style port sketched in `notes/PreparedStatementLogic.hs` is
**not** built for this release — it is a design project of its own, and the
scenarios it would enable are reachable against a real server.

## Consequences

- **Breaking.** `preparable`, `unpreparable` and `noPreparedStatements` all
  disappear. Every downstream package that constructs statements (hasql-th,
  hasql-implicits, hasql-dynamic-statements, hasql-interpolate, …) needs
  updating in step.
- **No per-statement opt-out.** Users on PostgreSQL 12+ have
  `plan_cache_mode = force_custom_plan`. Users on 9–11 have no per-statement
  recourse and must fall back to `statementCacheSize 0` connection-wide. This is
  the one accepted regression.
- **Server-side prepared statements become bounded** at `statement_cache_size`
  per connection, where today they are unbounded. Note the multiplier: 1024 ×
  pool size is the worst-case server-side plan count.
- **Hot statements pay one extra unprepared execution per connection** at the
  default threshold.
- **First admission gets faster in serial mode**: one roundtrip instead of two.
- **New dependency**: `psqueues`.
- The two new knobs ship with `statementCacheStats` so users can size them from
  evidence rather than guesswork.

## Out of scope

- The `Requirements` port for pure logic testing (see §8).
- Transaction-level retry of `0A000`/`26000` — arguably belongs in
  hasql-transaction, which already retries serialization failures.
- This repo has no `.haskell-coding-standards.lock`; reconciling it against the
  standards repo is separate work and is not a prerequisite here.
