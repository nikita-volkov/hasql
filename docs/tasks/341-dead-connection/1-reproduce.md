# Task 1 of 2: reproduce #341 in tests

Issue: <https://github.com/nikita-volkov/hasql/issues/341>. Branch: `v2.0`. This task changes tests only; the fix is [task 2](2-fix.md).

## The bug

When the connection dies between two sessions, the next session's send still succeeds because the bytes land in the kernel buffer. The receive then gets a `FatalError` result from libpq. `Hasql.Comms.ResultDecoder.serverError` decodes it as a server error, and `Hasql.Engine.Errors.fromRecvError` turns that into a catchable `StatementSessionError`. The reporter (AWS Aurora failover, TLS) got:

```haskell
StatementSessionError 1 0 "SELECT 1" [] True (ServerStatementError (ServerError "" "" Nothing Nothing Nothing))
```

libpq's own message ("SSL error: unexpected eof while reading") is never read. Pools keep the dead connection because only `ConnectionSessionError` tells them to drop it.

## Target behaviour

The tests encode the post-fix contract and are **red** on the current code:

> When the connection is gone by the time an operation receives its results, `Hasql.Connection.use` returns `Left (Errors.ConnectionSessionError details)`, where `details` is non-empty.

The contract applies whatever the server sent before closing, including a SQLSTATE. A further `use` on the same handle also returns `ConnectionSessionError`. That case is a guard and may already pass.

## Matrix

Each cell is one `it`. The shared hook runs the whole spec against both pqi adapters and both Postgres distros.

**Ways of dying:**

1. **Silent close.** The socket closes with a plain FIN (graceful close) and no protocol message. This is the reporter's shape, where libpq makes up a `FatalError` with no SQLSTATE. Reproduce it with an in-process TCP proxy:
   - Bind `127.0.0.1:0`.
   - Accept one client and dial the container's `host:port`.
   - Run one forwarding thread per direction.
   - Expose a `sever` action that gracefully closes both sockets and waits for the forwarders to stop.

   Acquire the hasql connection through the proxy by passing `(adapter, "127.0.0.1", proxyPort)` to `Scripts.onConnection`. A reset (RST) is the wrong fixture: it makes the *send* fail, and that path is already handled correctly.
2. **Server-announced termination.** This is stock Postgres's version of the same failure.
   - Read the victim's `pg_backend_pid()`.
   - From a second connection, run `pg_terminate_backend(pid)`.
   - Poll `pg_stat_activity` from the second connection until the pid is gone, so that the kill has landed before the victim runs anything.

   Avoid the two-argument `pg_terminate_backend`, because the hook also runs Postgres 9.

**Operations**, run as the victim's next session:

| Operation | Path it exercises |
|---|---|
| `Session.statement`, unpreparable | `queryParams` |
| `Session.statement`, preparable, first use | the `prepare` roundtrip |
| `Session.statement`, preparable, already run once before the kill | `queryPrepared` with the statement already in the cache |
| `Session.pipeline` of two statements | `toPipelineIO`, `Recv` `<*>` |
| `Session.script` | `Recv.allResults`, `fromRecvErrorInScript` |
| `Session.statement` whose codec names a type, e.g. an enum, so that its OID is looked up first | `SelectTypeInfo`, `fromRoundtripError` |

The type in the last row does not need to exist, because the lookup roundtrip fails before anything checks for it.

## Steps

1. **Write the spec** at `src/library-tests/Integration/Sharing/Connection/Use/LostConnectionSpec.hs`.
   - It is a case file in the sense of `src/library-tests/README.md`, so the proxy and kill fixtures live in it as top-level helpers.
   - Open the module with a haddock paragraph that states the contract and links #341.
   - Register the module in `other-modules` of `library-tests` in [hasql.cabal](../../../hasql.cabal), and add `network` to that suite's `build-depends`.
   - Make each failing `it` print the full actual result, so that the red output records today's misclassification.
2. **Run the spec**: `cabal test library-tests --test-options='--match LostConnection'`. Run it five times. Every cell should be red the same way each time. If a cell fails inconsistently, fix the fixture's ordering before you continue.
3. **Probe the raw libpq shape.** Use a throwaway executable or GHCi session against a test container, and do not commit it. For each fixture and adapter:
   - Send one `sendQueryParams` and call `getResult` until it returns `Nothing`.
   - For every call, record whether it returned `Just` or `Nothing`, plus `resultStatus`, `resultErrorField DiagSqlstate` and `resultErrorMessage`.
   - Also record `Pq.status` right after every call.

   This tells task 2 at which call the connection is first reported as bad. That timing may differ between the two fixtures and between `pqi-ffi` and `pqi-native`.
4. **Fill in [Findings](#findings).**
5. Run Ormolu on the new and changed `.hs` files.

## Done when

- All 12 cells (2 fixtures × 6 operations) are red on both adapters and both distros, and every failure message shows the actual error returned.
- If a cell is green on the current code, Findings records why, for example a send failure that is already classified correctly.
- Findings is filled in for all four fixture × adapter combinations.
- Every other suite (`library-tests` minus this spec, `comms-tests`, `connection-state-tests`) is as green as it was before.

## Findings

Measured on 2026-10-12 with `pqi-ffi` 1.0.1.0 (libpq) and the local `pqi-native` 1.0.1.13, against `postgres:9` and `postgres:18`. Results were identical on both distros everywhere, so the tables carry no distro column. The spec ran five times in a row with identical results.

### Summary

- All 12 cells (2 fixtures × 6 operations) are red on both adapters and both distros. The failure message shows the actual outcome.
- **`pqi-ffi`** fails as the issue describes: a dead connection is reported as `StatementSessionError`, `ScriptSessionError` or `DriverSessionError`.
- **`pqi-native` fails differently, and worse.** It never reports `ConnectionBad`, and `Pq.getResult` throws an `IOException` on EOF. `Connection.use` lets that escape. This is the stop-and-report case in the *Done when* of [task 2](2-fix.md); see [pqi-native](#pqi-native) below.
- The guard (a further `use` on the same handle) is green on `pqi-ffi` and red on `pqi-native`, where the call never returns.

### Actual errors per cell

`ReturnedOutcome (Left …)` is what `Connection.use` returned; `ThrewOutcome` is what it threw.

| Fixture | Operation | Adapter | Error returned today |
|---|---|---|---|
| Silent close | Unpreparable statement | pqi-ffi | `StatementSessionError 1 0 "select 1" [] False (ServerStatementError (ServerError "" "" Nothing Nothing Nothing))` |
| Silent close | Preparable statement, first use | pqi-ffi | `StatementSessionError 1 0 "select 1" [] True (ServerStatementError (ServerError "" "" Nothing Nothing Nothing))` |
| Silent close | Preparable statement, already in the statement cache | pqi-ffi | `StatementSessionError 1 0 "select 1" [] True (ServerStatementError (ServerError "" "" Nothing Nothing Nothing))` |
| Silent close | Pipeline of two statements | pqi-ffi | `StatementSessionError 2 0 "select 1" [] True (ServerStatementError (ServerError "" "" Nothing Nothing Nothing))` |
| Silent close | Script | pqi-ffi | `ScriptSessionError "select 1" (ServerError "" "" Nothing Nothing Nothing)` |
| Silent close | Statement with a named type | pqi-ffi | `DriverSessionError "Unexpected error outside of statement context. This indicates a bug in Hasql or the server misbehaving. Error: ServerError \"\" \"\" Nothing Nothing Nothing"` |
| Server-announced termination | Unpreparable statement | pqi-ffi | `StatementSessionError 1 0 "select 1" [] False (UnexpectedRowCountStatementError 1 1 1)` |
| Server-announced termination | Preparable statement, first use | pqi-ffi | `StatementSessionError 1 0 "select 1" [] True (UnexpectedRowCountStatementError 1 1 1)` |
| Server-announced termination | Preparable statement, already in the statement cache | pqi-ffi | `StatementSessionError 1 0 "select 1" [] True (UnexpectedRowCountStatementError 1 1 1)` |
| Server-announced termination | Pipeline of two statements | pqi-ffi | `StatementSessionError 2 0 "select 1" [] True (ServerStatementError (ServerError "57P01" "terminating connection due to administrator command" Nothing Nothing Nothing))` |
| Server-announced termination | Script | pqi-ffi | `ScriptSessionError "select 1" (ServerError "" "" Nothing Nothing Nothing)` |
| Server-announced termination | Statement with a named type | pqi-ffi | `DriverSessionError "Unexpectedly got too many results outside of statement context. This indicates a bug in Hasql or the server misbehaving. Amount: 1"` |
| Silent close | all six operations | pqi-native | `ThrewOutcome pqi-native: connection closed by server: end of file` |
| Server-announced termination | all six operations | pqi-native | `ThrewOutcome pqi-native: connection closed by server: end of file` |
| both | further use of the same handle | pqi-ffi | green: `ConnectionSessionError` with non-empty details, as the guard expects |
| both | further use of the same handle | pqi-native | `HungOutcome`: the call does not return within 5 s |

Why the `pqi-ffi` termination column differs from the silent-close one: libpq returns the real `57P01` FATAL first and, on a second `getResult`, a made-up `FatalError`. `singleResult` calls `getResult` twice, so the second result is read as "too many results" (`UnexpectedRowCountStatementError 1 1 1`, or the `DriverSessionError` above). The pipeline surfaces the `57P01` server error because first-error-wins keeps the first result's error.

No cell is green on current code except the `pqi-ffi` guard, which was expected to pass.

### Raw libpq sequence

One `sendQueryParams "select 1"`, followed by `getResult` until `Nothing`. `Pq.status` is read right after every call. Both distros gave the same sequence.

| Fixture | Adapter | Call # | Just/Nothing | resultStatus | SQLSTATE | resultErrorMessage | Pq.status after |
|---|---|---|---|---|---|---|---|
| Silent close | pqi-ffi | send | `True` | - | - | - | ConnectionOk |
| Silent close | pqi-ffi | 1 | Just | FatalError | Nothing | `"server closed the connection unexpectedly\n\tThis probably means the server terminated abnormally\n\tbefore or while processing the request.\n"` | **ConnectionBad** |
| Silent close | pqi-ffi | 2 | Nothing | - | - | - | ConnectionBad |
| Server-announced termination | pqi-ffi | send | `True` | - | - | - | ConnectionOk |
| Server-announced termination | pqi-ffi | 1 | Just | FatalError | `57P01` | `"FATAL:  terminating connection due to administrator command\n"` | ConnectionOk |
| Server-announced termination | pqi-ffi | 2 | Just | FatalError | Nothing | `"server closed the connection unexpectedly\n\tThis probably means the server terminated abnormally\n\tbefore or while processing the request.\n"` | **ConnectionBad** |
| Server-announced termination | pqi-ffi | 3 | Nothing | - | - | - | ConnectionBad |
| Silent close | pqi-native | send | `True` | - | - | - | ConnectionOk |
| Silent close | pqi-native | 1 | **throws** `IOException "pqi-native: connection closed by server: end of file"` | - | - | - | ConnectionOk |
| Server-announced termination | pqi-native | send | `True` | - | - | - | ConnectionOk |
| Server-announced termination | pqi-native | 1 | Just | FatalError | `57P01` | `"FATAL:  terminating connection due to administrator command\n"` | ConnectionOk |
| Server-announced termination | pqi-native | 2 | **throws** `IOException "pqi-native: connection closed by server: end of file"` | - | - | - | ConnectionOk |

For task 2:

- **Timing of `ConnectionBad` on `pqi-ffi`.**
  - Silent close: bad from the first `getResult`.
  - Termination: still `ConnectionOk` after the first result, and bad only after the second `getResult`. So "check `Pq.status` after the results have been consumed" is right, and "check the first result" would miss the termination case.
  - `singleResult` makes exactly the two calls needed, so the status is already bad when it checks.
  - `allResults` loops until `Nothing`, so it sees it too.
- **Message on `pqi-ffi`.** libpq's own text is present on the made-up `FatalError` (`resultErrorMessage`) and in `Pq.errorMessage connection`. On termination `Pq.errorMessage` concatenates the FATAL text and the "server closed the connection" text.
- **The probe ran the termination fixture directly**, without the proxy that the spec now uses (see below). The spec's per-cell errors agree with the sequence above.
- **The probe did not repeat `getResult` after `pqi-native` threw**, so it is not known whether the next call throws again.

### pqi-native

This is a different problem from #341 as the reporter saw it, and it blocks the *Done when* of task 2 for every `pqi-native` cell:

1. **`Pq.status` stays `ConnectionOk`** after the EOF, even after `getResult` has thrown. The rule in task 2 cannot fire.
2. **`Pq.getResult` throws** instead of returning a `FatalError` result. libpq never throws here, and `Hasql.Comms.Recv` assumes it does not.
3. **`Connection.use` lets the exception escape.** That breaks "no exceptions in the public API".
4. **The handle is left unusable.** After the throw, a further `use` never returns, and `Connection.release` never returns either. `release` blocking made the whole `library-tests` run die with `BlockedIndefinitelyOnMVar` in the first attempt, before the spec learned to bound it. From reading `Connection.use`, the likely cause is that `cleanUpAfterInterruption` runs under `mask`, throws the same exception from its own `getResult`, and so never reaches the `putMVar` that returns the state to the handle. This is inferred from the code and the observed hangs, not directly observed.

Per task 2, items 1 and 2 are a `pqi-native` bug and are not to be worked around in hasql. Item 4 is a hasql defect that is independent of the adapter, in that any `getResult` that throws during cleanup leaves the handle empty. It needs a decision from the maintainer on whether it belongs in task 2.

### Fixture notes

- **Silent close.** The proxy half-closes: it shuts down the database socket, the forwarder passes the EOF on to the client as a FIN, and the client-facing socket stays open so the victim's next send is swallowed instead of reset.
- **Server-announced termination is also routed through the proxy.** This is a deviation from the task text. The kill is still `pg_terminate_backend` from a second connection, polled out of `pg_stat_activity`. Direct connections were not deterministic:
  - In the first full run one `pqi-native` cell (`postgres:18`, termination, pipeline of two statements) was green, and red in the four runs after it. Green means the call returned a non-empty `ConnectionSessionError`, i.e. it took the already-correct send-failure path.
  - A loop of 40 direct attempts of that cell gave 38 × `connection closed by server: end of file` and 2 × `Network.Socket.recvBuf: resource vanished (Connection reset by peer)`. The dead server answers the victim's write with a reset, which races with reading the FATAL message.
  - The send-failure path is the one the task text already excludes for the silent close. With the proxy as a buffer, five consecutive runs gave identical results.
- **Bounded waits in the spec.** A call to `Connection.use` is bounded to 5 s and the victim's release to 1 s. Without that, the `pqi-native` hang above stops the run instead of failing the cell.
