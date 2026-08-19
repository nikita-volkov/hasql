# hasql-pool: work required by the connection-finishing change

`hasql-pool` 1.5.0.1 compiles against this branch unchanged - no API broke. Its
integration suite does not pass.

Reproduced against `hasql-pool` at `50b4986` with this branch supplied as a
local package:

```
42 examples, 4 failures
```

Two distinct specs, each failing once per adapter (`pqi-ffi`, `pqi-native`).

The tasks below are `hasql-pool`'s except where marked. Move this file into that
repository when the work starts.

---

## 1. Discard the connection when a session is interrupted (required)

Failing spec: `Specs.Use` → *"Does not return a connection to the pool when the
session is interrupted by an asynchronous exception"*
(`src/integration-tests/Specs/UseSpec.hs:90`).

```
predicate failed on: Left (SessionUsageError (ConnectionSessionError "The connection has been released"))
```

`onLiveConn` in `src/library/Hasql/Pool.hs` handles an exception out of
`Connection.use` like this:

```haskell
sessRes <- try @SomeException (Connection.use (entryConnection entry) sess)

case sessRes of
  Left exc -> do
    returnConn
    throwIO exc
```

`returnConn` writes the entry back onto `poolConnectionQueue`. As of this branch
the driver has finished that connection on its way out, so the entry re-queued
here reaches a spent handle. The next caller to draw it gets
`SessionUsageError (ConnectionSessionError …)` for a session that never had
anything wrong with it, and the failure is attributed to whoever drew the
connection rather than to whoever cancelled.

The spec's own comment already describes this branch as a bug and names it a
plausible root cause of the "connection pointer is NULL" reports. It passed
before only because the driver quietly repaired the connection behind the pool's
back. That repair is what this branch removes.

Fix: on the exception path, take the discard branch rather than `returnConn` -
`Connection.release` the entry (now a no-op when the driver already finished it,
so it is safe on every route), `atomically $ modifyTVar' poolCapacity succ`, and
emit the termination observation - then rethrow.

Note the accounting: `returnConn` does *not* increment `poolCapacity` when it
re-queues, because the entry keeps occupying its slot. The discard path must
increment, exactly as the `requiresConnectionDiscard` branch below it does.
Getting this wrong leaks pool capacity on every cancelled session, which shows
up as `AcquisitionTimeoutUsageError` rather than as anything pointing back here.

## 2. `initSession` state lost to an exception that touched nothing (hasql-side question)

Failing spec: `Specs.Config.InitSession` → *"Persists after exceptions thrown in
session"* (`src/integration-tests/Specs/Config/InitSessionSpec.hs:27`).

```
expected: Right (Just "1")
 but got: Left (SessionUsageError (ConnectionSessionError "The connection has been released"))
```

The session under test is `liftIO (throwIO (userError …))` and nothing else. No
command reaches the connection, which is idle, clean, and outside a transaction
when the exception fires - and the driver finishes it anyway, taking the
`initSession` state with it.

Task 1 does not fix this one: the pool discards a connection the driver already
finished either way. The question is whether the driver should have finished it.

Two ways to settle it, and the choice is hasql's:

- **Accept it.** The current policy is a flat rule - any exception out of a
  session spends the connection - and `Integration.Sharing.Connection.Use.InterruptionSpec`
  asserts it in that form. The pool spec then needs rewriting to expect a fresh
  connection without the setting, and the `initSession` guarantee gets documented
  as "survives errors, not exceptions".

- **Narrow the rule to interruptions that landed mid-round-trip.** On the
  exception path, hand the connection back when `PQtransactionStatus` reports
  `Idle` *and* `PQpipelineStatus` reports the mode off; finish it otherwise. Both
  are local, non-blocking reads, so the reason the repair was removed - blocking
  network IO under a mask - does not apply to them.

  This is a policy change, not a bug fix, and it is not free. It needs
  `InterruptionSpec` rewritten (its "An exception thrown by the session → Spends
  the handle" case throws after a completed `script "select 1"`, i.e. from an
  idle connection, and would start passing the connection back). It also rests on
  both `pqi` backends reporting those two statuses as faithfully as libpq does -
  `pqi-native` maintains `txStatus` in an `IORef` of its own, which has to be
  confirmed to go `Active` while a command is outstanding, including one merely
  queued in pipeline mode. Guess wrong there and poisoned connections start
  reaching pools again, which is the exact class of bug this branch closes.

Until this is decided, task 1 stands on its own and this spec stays red.

## 3. Refresh the stale prose

Neither is a behaviour change; both now describe something that does not exist.

- `src/library/Hasql/Pool/SessionErrorDestructors.hs`, the comment above the
  `DriverSessionError` case: *"Hasql closes the libpq connection when cleanup
  after an interruption fails, so it must not be reused by the pool."* There is
  no cleanup after an interruption any more. The conclusion survives - hasql
  closes the connection before returning either `DriverSessionError` or
  `ConnectionSessionError`, so both must be discarded - but the reason has
  changed from "the repair failed" to "there is no repair".

- The haddock on `Hasql.Pool.use` refers to `Session.ClientError`, a constructor
  that no longer exists in hasql. The behaviour it describes is now keyed on
  `requiresConnectionDiscard`.

## 4. Confirm double-`finish` tolerance across both backends

Not currently failing - `Specs.Use` → *"Connection errors cause eviction of
connection"* passes on both adapters - but the branch leans on it in a new
place, so it is worth stating as an invariant rather than an accident.

`Helpers.Sessions.closeConn` calls `Pqi.finish` inside `onLibpqConnection` and
returns `Right` with the same connection. The driver therefore does not mark the
connection spent, hands it back, and finishes it itself once the next statement
fails on it - a second `finish` on the same connection. `pqi-native` handles this
explicitly (it flips `connStatus` to `ConnectionBad` so later operations are
rejected rather than attempted on a closed socket). The `pqi-ffi` path inherits
whatever `postgresql-libpq` does.

What changed is the direction of the pressure: `Connection.release` is now a
no-op on an already-finished handle, so the pool no longer double-finishes, but
the driver's own finish-on-verdict can. Worth a conformance case in `pqi` rather
than a spec here.
