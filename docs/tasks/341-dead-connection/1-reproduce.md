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

<!-- Filled in by step 4. -->

### Actual errors per cell

| Fixture | Operation | Adapter | Error returned today |
|---|---|---|---|

### Raw libpq sequence

| Fixture | Adapter | Call # | Just/Nothing | resultStatus | SQLSTATE | resultErrorMessage | Pq.status after |
|---|---|---|---|---|---|---|---|
