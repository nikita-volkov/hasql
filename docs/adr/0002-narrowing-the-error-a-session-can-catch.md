# Narrowing the error a session can catch

`Session` keeps its `MonadError` instance. What changes is the type that instance ranges
over: `SessionError` is narrowed to failures that leave the connection usable, and every
connection-fatal failure moves into a new outer type that `catchError` structurally cannot
reach.

## Where the instance came from

It was derived, not designed. `Session` is declared

```haskell
newtype Session a
  = Session (ConnectionState -> IO (Either SessionError a, ConnectionState))
  deriving (Functor, Applicative, Monad, MonadError SessionError, MonadIO)
    via (ExceptT SessionError (StateT ConnectionState IO))
```

and `MonadError` came along with the transformer stack that happened to model the
implementation. The tests that pin its behaviour were written afterwards, against whatever
it turned out to do, and the driver has since been shaped around it: pipeline-mode
restoration lives inside `Hasql.Comms.Roundtrip.toPipelineIO` rather than in
`Hasql.Connection.use` precisely because a session can catch a pipeline failure and arrange
for the repair never to run.

`MonadIO` is derived on the same line and does not carry the same problem. A user who raises
an exception inside a session leaves through `use`'s exception path, which runs
`cleanUpAfterInterruption`: cancel, drain, `ABORT`, `DEALLOCATE ALL`. That path is safe by
construction. `catchError` is the only route by which a user resumes on a connection the
driver has already given up on, with no repair running at all, and it is the only one that
can turn such a failure into a `Right` that `use` treats as a clean return.

## The defect

It is not that a session can recover. It is that `SessionError` has no health axis, so
`catchError` cannot tell "the server rejected this statement" from "the socket is gone".
Those two demand opposite treatment and the type puts them in one place.

Catching a unique violation to fall back is legitimate and common. Catching a lost
connection and issuing the next statement is nonsense that the type permits, and the cost
does not land on whoever wrote it: the session returns `Right`, `use` hands the connection
back, and the next borrower of a pooled connection inherits the damage. That is the whole of
[hasql-pool#35](https://github.com/nikita-volkov/hasql-pool/issues/35).

So give the error type the axis it was missing. Fix the type and the instance becomes
defensible, because there is nothing unrecoverable left for it to offer recovery from.

## The types

`ConnectionError` is renamed to `AcquireError`, so the two error types are named for the two
operations rather than one for an operation and one for a resource.

`SessionError` keeps its name and loses its fatal constructors. It now means exactly one
thing: something went wrong and the connection is still usable. The name still describes it
accurately, and the change of meaning is what a major release announces.

```haskell
data UseError
  = SessionUseError SessionError
  | ConnectionUseError ...
  | DriverUseError ...
```

`use` returns `Either UseError a`. `catchError` ranges over `SessionError` only, so it is
total over its own type, and the residual is a different type the caller is forced to handle
at `use`.

Read the three constructors as one sentence: `SessionUseError` means reuse the connection,
the other two mean discard it. Health is the only axis that separates the catchable from the
uncatchable. Provenance separates nothing except the two fatal constructors from each other,
and there it is diagnostic. So `MissingTypesSessionError` stays a `SessionError` despite
originating in the driver rather than in the server.

`isTransient` follows from the constructors instead of from a classifier. `ConnectionUseError`
is transient, because a lost socket is worth another connection. `DriverUseError` is not,
because the same request or the same driver fault reproduces on any connection.
`SessionUseError` delegates. That is changelog #327's fix expressed in the type, so it can no
longer drift.

`toSqlState` on `UseError` delegates for `SessionUseError` and answers `Nothing` for the
other two. Changelog #325 records this delegation as the thing that gets forgotten whenever a
wrapping type is added, which is reason enough to write it down here.

## Classifying a failure

Two rules, one per direction.

**A send that fails is fatal, always.** The driver asked libpq to put bytes on the wire and
libpq refused, so the driver stops vouching for the connection whatever the reason.
`Errors.fromSendError` survives unchanged and only retargets: the `connectionLost` bit that
`runSend` already reads off `Pq.status` selects `ConnectionUseError` when the socket is gone
and `DriverUseError` when libpq refused the request itself. Both finish the connection.

This is uniform across serial and pipeline mode. It costs something and the cost is accepted:
a caller that repeatedly submits a batch with more than 65535 parameters loses a connection
each time, statement cache included, rather than getting an error back on a connection that
libpq still reports as fine. A refused send means the driver's own send path did not do what
the driver expected, and a connection whose send path has behaved unexpectedly is not one to
hand to the next session.

**A receive is classified by connection health.** A server rejecting a statement leaves the
connection idle and usable, so it stays a `SessionError`. A receive that failed with the
connection reported bad is a `ConnectionUseError`.

`Recv` does not currently collect what that needs. `runSend` consults `Pq.status` after a
failure and carries the verdict; `Recv` never consults it, so a socket dying mid-receive
makes `PQgetResult` return `Nothing`, `Recv.singleResult` produces `NoResultsError`, and
`fromRecvError` maps that to `StatementSessionError … (UnexpectedRowCountStatementError 1 1
0)`. A dead connection reports itself as "expected 1 row, got 0". Under the split that value
would be a `SessionError`, catchable, asserting a healthy connection that is gone, so the
guarantee this whole design exists to establish would break on the receive path rather than
the send path.

`Recv.Error` therefore gains the same `connectionLost` bit, read the same way and at the same
moment. One mechanism on both sides.

This depends on a property of the current `Recv` combinators worth stating, because nothing
enforces it: they all drain to `Nothing` before they classify. `singleResult` issues its
second `getResult` ahead of decoding and `allResults` loops to the end, so a server that
reports a terminal condition and closes in an orderly way has already produced EOF by the
time the status is read, and libpq reports the connection bad rather than optimistically
fine. A future combinator that classifies before draining would not get that for free.

## How a fatal error gets past a handler

The newtype carries the outer type:

```haskell
newtype Session a
  = Session (ConnectionState -> IO (Either UseError a, ConnectionState))
```

and `MonadError SessionError` is written by hand. `throwError` constructs only the
`SessionUseError` branch, `catchError` inspects only that branch, and the other two flow past
every handler untouched. Uncatchability becomes a property of the `Monad` instance rather
than of a handler's care, which is the point: it is not a rule anyone can forget to follow.

Hand-writing four instances that were derived is the tax on having decided what they should
mean. The `via` clause is what produced the wrong meaning in the first place, so keeping it
is not a saving.

## What `use` does on each path

| Outcome | `use` |
|---|---|
| `Right a` | nothing |
| `Left (SessionUseError _)` | nothing |
| `Left (ConnectionUseError _)` | finish the connection |
| `Left (DriverUseError _)` | finish the connection |
| exception | `cleanUpAfterInterruption`, unchanged |

Repair happens only where the driver lost control. A session that returns `Right` or
`SessionUseError` returned from a completed receive sequence, so nothing is in flight and
there is nothing the driver both needs to and can put right. `cleanUpAfterFailure` and its
unconditional call on the `Left` path go.

`toPipelineIO`'s `leavePipeline` stays, and its remaining job is narrower than it was. Send
failures no longer need it, since the connection is finished either way. It is load-bearing
for the receive-side and server-error paths, which stay catchable and therefore still need
the mode off before the session continues.

Note that the exception path is not the user's negligence being corrected. It handles async
exceptions too, a `timeout` or a `killThread` landing mid-command, and `use` cannot tell those
from a `throwIO` the session made itself. Its job there is to bring a connection whose
protocol state is indeterminate back to a known one, and the `ABORT` is a consequence of that
rather than a position on anyone's unit of work.

Two things are consequently the caller's responsibility and are not policed.

A transaction left open across a caught error is one. Whoever composed the transaction closes
it. `use` returns `Right` on a connection sitting in `TransInError` and nothing distinguishes
that from a clean return, so hasql-pool#35 stays open for callers who catch inside a
hand-rolled transaction, and a pool that wants to act on it reads the status through
`onLibpqConnection`.

A pipeline left open by `onLibpqConnection` is the other, and it is the sharper of the two.
Per `toPipelineIO`, such a connection refuses serial commands and hands the next pipeline the
stale results of this one, which is silent cross-session corruption rather than a loud
failure. Dropping the `Left`-path repair leaves it unguarded. That narrows what hasql promises
about `onLibpqConnection` and belongs in the changelog as such.

## The state a caught error keeps

A caught error retains every connection-state change made before it. The newtype returns the
state on both branches, which is `ExceptT e (State s)` and not `StateT s (Either e)`, and the
difference is not academic: a statement whose `PARSE` succeeded and whose `EXECUTE` failed has
truthfully been prepared on the server, so discarding the cache entry would cost a `42P05`
round trip to rediscover it, and discarding resolved OIDs would cost a `SelectTypeInfo` each.

That retention carries an obligation, and the obligation is the half worth writing down,
because it constrains code that does not exist yet and is invisible from every call site: no
operation may record an effect it has not confirmed. `Session.statement` honours it by
reverting the statement cache except on a prepare collision, and `Pipeline.run` does the same
across partial progress. Both belong in the instance's haddock, alongside the statement of
what `catchError` cannot see, since a chosen meaning nobody wrote down is a slower kind of
accident.

## What stays and why

`MonadIO` stays as it is. `MonadError` was worth attacking because it misrepresented itself,
presenting recovery as safe where it was not. `liftIO` claims arbitrary IO happens and that is
what happens. Removing it would cost logging and metrics inside a session and close nothing,
since `onLibpqConnection` has to stay regardless.

`MonadError` stays a class rather than becoming a pair of plain functions. The reasons it
looked questionable were all consequences of it being wrong-typed. As a class it lifts through
user transformer stacks for free, so `ReaderT AppEnv Session` inherits catching, and
`tryError`, `handleError` and `liftEither` work off the shelf. Plain functions cannot reproduce
that.

`throwError` stays, with no warning attached. On the flat type a minted error was a claim about
connection health that the driver's own machinery would act on, and that made it a hole.
Narrowed, every value a user can mint asserts what happened and never what state anything is
in, so no false premise is left for anything to act on. `Hasql.Errors` exports
`SessionError (..)` anyway, because pattern matching is the type's whole purpose, so removing
`throwError` would close nothing while costing a rethrow that transaction libraries need in
order to roll back and re-raise. Application-level failure conditions still belong in
`ExceptT MyError Session`, where they stay the application's, but that is a recommendation and
not a caution.

## Rejected alternatives

1. **Cut at `ServerError`.** Narrow the instance to `MonadError ServerError`, so only
   server-reported failures are catchable. It excludes the fatal set correctly and excludes too
   much else with it: `UnexpectedRowCountStatementError`, row and cell decoding failures,
   `UnexpectedColumnTypeStatementError` and `MissingTypesSessionError` all leave the connection
   idle and healthy. Cutting on provenance answers nothing when a user asks why a unique
   violation is catchable and a null in column 3 is not.
2. **A predicate on the flat type.** Keep `SessionError` as it is and add `isConnectionHealthy`
   beside `isTransient`, with the recovery path consulting it. That documents the invariant
   instead of enforcing it, and predicates drift from the constructors they classify.
   `isTransient` did exactly that, hard-coded to `False` on `ServerError` until changelog #325.
3. **A parallel narrow type.** Keep `SessionError` flat for reporting and give the recovery path
   its own `RecoverableError` with a total injection back. Two types with overlapping constructor
   sets have to be kept in sync by hand forever.
4. **A private exception for fatal failures.** Let `SessionError` narrow, and have the driver
   throw fatal failures rather than return them, so `catchError` cannot see them because it
   cannot see exceptions. It works, and it puts driver-fatal failures in the same channel as
   async exceptions and user `throwIO`, so `use` has to sort them apart by exception type. That
   reintroduces the provenance sniffing the split exists to remove, and it moves an error the
   driver can describe precisely out of the value channel where the rest of hasql's errors live.
5. **Keeping a refused send catchable.** libpq reports the connection fine after refusing a
   request on its own merits, so health alone would leave it a `SessionError` and spare the
   caller a reconnect. Rejected because the rule then stops being a rule: every send site would
   carry a judgement about which refusals are benign, and the connection would be handed on with
   the driver having seen its send path behave in a way it does not model.
6. **Classifying a pipeline send failure by whether the repair worked.** `leavePipeline` makes a
   best-effort recovery whose outcome is discarded, so reporting it would let a repaired
   connection stay catchable. Rejected with the same rule: the send failed, and that is the
   verdict. It also keeps serial and pipeline mode from needing separate classification.
7. **Discarding state on a caught error.** `StateT s (Either e)` rather than `ExceptT e (State s)`.
   Every OID resolved and every statement prepared before the failure would be thrown away and
   re-fetched, and the caches are monotonic and connection-scoped, so there is nothing the
   rollback would be protecting.

## What breaks

Release 2.1, no shim, no compatibility aliases and no pattern synonyms. The constructor split is
the point of the change, and synonyms that let an old `case` keep compiling would preserve
precisely the code that has to be reconsidered. A handler matching on `ConnectionSessionError`
inside a session is unreachable by construction now, and its author needs to see it break rather
than have it quietly typecheck.

| Site | Effect |
|---|---|
| `CatchErrorSpec` | survives, `script "absurd"` is a server error |
| Hstore and Citext `CREATE EXTENSION` probes | survive, server errors |
| `PipelineSpec` catch cases | survive, server errors |
| `RecoveredPipelineFailureSpec` | rewritten, see below |
| hasql-transaction | rollback-and-rethrow keeps working |
| hasql-pool | `UseError` in signatures, #35 unchanged |

`RecoveredPipelineFailureSpec` has to be rewritten rather than adjusted. Its premise is that a
session recovers from a pipeline send failure and carries on, and it asserts that pipeline mode
is off for the remainder. A failed send is uncatchable now, so the `catchError` never fires and
there is no remainder to assert about. What survives of it is the obligation it was written to
protect, which `toPipelineIO` discharges on every path, so the replacement asserts on
`toPipelineIO` directly instead of through a recovery that can no longer happen.

The receive-side classification wants a test of its own, and it does not have one today: a
connection killed mid-receive, from another session or by terminating the backend, has to come
back as `ConnectionUseError` rather than as a row-count complaint.

hasql-transaction is unaffected because it catches only what it should. A body failing with a
`SessionError` is caught, rolled back and rethrown. A body failing connection-fatally is not
caught at all, which is correct, since there is no connection left to roll back on.

Implement in this order: split the error types first and let the compiler enumerate the call
sites, then give `Recv` its status bit, then hand-write the instances against the split, and
delete the `Left`-path repair last, when nothing catchable can reach it any more.
