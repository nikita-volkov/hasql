# Narrowing the error a session can catch

`Session` keeps its `MonadError` instance. What changes is the type that instance ranges
over: `SessionError` is narrowed to failures that leave the connection usable, and every
connection-fatal failure moves into a new outer type that `catchError` structurally cannot
reach.

[ADR 0001](0001-connection-death-is-state-not-a-returned-error.md) settles where the verdict
"this connection is spent" lives, and settles it against its own preference: the verdict sits
in `ConnectionState.dead` rather than being derived from the error `use` gets back, because
`MonadError` lets a session catch that error and discard it. It then names the ways out -
"splitting the error type so the fatal cases travel on a channel `catchError` does not cover"
among them. This is that split, written out.

## What the flag bought, and what it did not

The flag closed the safety hole. A session that catches a connection-fatal failure now has
every later operation refused by the guard in `Hasql.Engine.Contexts.Session.onConnectionState`,
and `Hasql.Connection.use` finishes the connection whichever way the session ends - including
when it swallows the failure and returns `Right`. Nothing poisoned reaches a pool any more, and
`Integration.Sharing.Session.RecoveredPipelineFailureSpec` pins all three cases.

What the flag did not do is stop `catchError` from offering the recovery. It only made the
recovery inert. So the type still says a lost socket is something a handler may take over from,
and four separate pieces of machinery exist to make sure nothing comes of it when a handler
believes it:

- `ConnectionState.dead` and `ConnectionState.setDead`, a field on the state that is not about
  the connection's contents.
- `onConnectionState`'s short-circuit, which is a hand-rolled `>>=` for the case where the real
  `>>=` was talked out of short-circuiting.
- `Session.deadConnectionError`, the anonymous error that short-circuit reports - "The driver
  has given up on this connection earlier in the session", naming neither the failure nor the
  statement, which is half of
  [spent-connection errors name no cause](../problems/spent-connection-errors-name-no-cause.md).
- The second net in `use`, `either connectionIsSpent (const False) result`, which exists for the
  errors that never passed through `onConnectionState` at all - a session minting one with
  `throwError`, for one.

None of it is wrong. All of it is the cost of a `Left` that a handler is invited to remove.

There is a live regression in that invitation too, and it is not hypothetical.
`hasql-transaction`'s `tryTransaction` catches whatever the transaction body reports and issues
an `ABORT` before classifying it. When the body failed connection-fatally, the driver refuses
that `ABORT`, the handler fails on the refusal, and the original error is dropped in favour of
`deadConnectionError`. The catch is `hasql-transaction`'s to fix, but the type is what told it
the catch was reasonable.

## Where the instance came from

It was derived, not designed. `Session` is declared

```haskell
newtype Session a
  = Session (ConnectionState -> IO (Either SessionError a, ConnectionState))
  deriving (Functor, Applicative, Monad, MonadError SessionError, MonadIO)
    via (ExceptT SessionError (StateT ConnectionState IO))
```

and `MonadError` came along with the transformer stack that happened to model the
implementation. The tests that pin its behaviour were written afterwards, against whatever it
turned out to do, and the driver has since been shaped around it twice. First pipeline-mode
restoration moved out of `Hasql.Connection.use` and into `Hasql.Comms.Roundtrip.toPipelineIO`,
because a session can catch a pipeline failure and arrange for a repair that runs after the
session never to run at all. Then repair was deleted outright and the verdict moved into the
state, for the same reason at one remove: what the session cannot intercept is not the repair
but the fact.

`MonadIO` is derived on the same line and does not carry the same problem. A user who raises an
exception inside a session leaves through `use`'s exception path, which finishes the connection
and touches nothing else. That path is safe because it declines to reason about the connection
at all. `catchError` is the only route by which a user resumes on a connection the driver has
already given up on, and the only one that can turn such a failure into a `Right` that `use`
would otherwise treat as a clean return.

## The defect

It is not that a session can recover. It is that `SessionError` has no health axis, so
`catchError` cannot tell "the server rejected this statement" from "the socket is gone". Those
two demand opposite treatment and the type puts them in one place.

Catching a unique violation to fall back is legitimate and common. Catching a lost connection is
nonsense that the type permits, and every consequence listed above follows from permitting it.
That the consequences are now contained rather than catastrophic is the flag's doing, not the
type's.

So give the error type the axis it was missing. Fix the type and the instance becomes
defensible, because there is nothing unrecoverable left for it to offer recovery from - and the
containment machinery has nothing left to contain.

## The types

`ConnectionError` is renamed to `AcquireError`, so the two error types are named for the two
operations rather than one for an operation and one for a resource.

`SessionError` keeps its name and loses its fatal constructors. It now means exactly one thing:
something went wrong and the connection is still usable. The name still describes it accurately,
and the change of meaning is what a major release announces.

```haskell
data UseError
  = SessionUseError SessionError
  | ConnectionUseError ...
  | DriverUseError ...
```

`use` returns `Either UseError a`. `catchError` ranges over `SessionError` only, so it is total
over its own type, and the residual is a different type the caller is forced to handle at `use`.

Read the three constructors as one sentence: `SessionUseError` means reuse the connection, the
other two mean discard it. Health is the only axis that separates the catchable from the
uncatchable. Provenance separates nothing except the two fatal constructors from each other, and
there it is diagnostic. So `MissingTypesSessionError` stays a `SessionError` despite originating
in the driver rather than in the server.

`Errors.connectionIsSpent` disappears into the constructors. It is a three-line classifier today
and correct today, but it is the kind of function that drifts from the type it classifies - which
is exactly what `isTransient` did, hard-coded to `False` on `ServerError` until changelog #325.
After the split there is nothing to keep in sync: the question it answers is which constructor
you are looking at.

`isTransient` follows the same way. `ConnectionUseError` is transient, because a lost socket is
worth another connection. `DriverUseError` is not, because the same request or the same driver
fault reproduces on any connection. `SessionUseError` delegates.

The caveat the merge added to `isTransient`'s haddock survives the split unchanged, because it is
not about the type. `use` finishes the connection before returning either fatal constructor, so
the transience is a claim about the operation and never about the `Connection` that reported it:
a retry loop must acquire a connection between attempts, and one that reuses the same handle
spins on an error that is transient forever. That stays prose, since no arrangement of
constructors can say it.

`toSqlState` on `UseError` delegates for `SessionUseError` and answers `Nothing` for the other
two. Changelog #325 records this delegation as the thing that gets forgotten whenever a wrapping
type is added, which is reason enough to write it down here.

`onLibpqConnection` is retyped to return `Either UseError a`. It is the one place a user can
genuinely wreck the connection, so it is the one place a user needs the vocabulary to say so -
and `Integration.Sharing.Session.OnLibpqConnection.DirtyReturnCleanupSpec` already depends on
being able to. Everywhere else, narrowing to `SessionError` is the point.

## Classifying a failure

Two rules, one per direction. Both are implemented as of the merge; the split only retargets
them.

**A send that fails is fatal, always.** The driver asked libpq to put bytes on the wire and libpq
refused, so the driver stops vouching for the connection whatever the reason. `Errors.fromSendError`
survives unchanged: the `connectionLost` bit that `runSend` reads off `Pq.status` selects
`ConnectionUseError` when the socket is gone and `DriverUseError` when libpq refused the request
itself. Both finish the connection.

This is uniform across serial and pipeline mode. It costs something and the cost is accepted: a
caller that repeatedly submits a batch with more than 65535 parameters loses a connection each
time, statement cache included, rather than getting an error back on a connection that libpq
still reports as fine. A refused send means the driver's own send path did not do what the driver
expected, and a connection whose send path has behaved unexpectedly is not one to hand to the
next session.

**A receive is classified by connection health.** A server rejecting a statement leaves the
connection idle and usable, so it stays a `SessionError`. A receive that failed with the
connection reported bad is a `ConnectionUseError`.

`Recv.Error` carries the same `connectionLost` bit as `Roundtrip.ClientError`, read the same way
from `Pq.status`, on `NoResultsError`. One mechanism on both sides. Without it a socket dying
mid-receive made `PQgetResult` return `Nothing`, `Recv.singleResult` produce `NoResultsError`,
and `fromRecvError` map that to `StatementSessionError … (UnexpectedRowCountStatementError 1 1 0)`:
a dead connection reporting itself as "expected 1 row, got 0". Under the split that value would
be a `SessionError`, catchable, asserting a healthy connection that is gone, so the guarantee
this whole design exists to establish would have broken on the receive path rather than the send
path.

The bit is now read at every point where the result stream terminates, including the terminations
that follow a result the driver already holds. That is broader than health alone requires, and
[nothing can say "it succeeded and the connection is gone"](../problems/no-channel-for-succeeded-but-spent.md)
is the record of the trade: a command that ran and committed, on a socket that died between
`CommandComplete` and `ReadyForQuery`, is reported as a connection failure and its result
discarded.

The split does not settle that, and cannot. `use` returns `Either UseError a`, one slot, so
"succeeded and the connection is gone" has nowhere to go in the error type however the error type
is arranged. Should that problem ever be fixed the way its own document proposes - a status check
on the success path, setting a bit on the connection state - then a `dead`-shaped field comes
back, for a reason that has nothing to do with catchability. Worth stating so that nobody reads
this ADR as having closed that door.

## How a fatal error gets past a handler

The newtype carries the outer type:

```haskell
newtype Session a
  = Session (ConnectionState -> IO (Either UseError a, ConnectionState))
```

and `MonadError SessionError` is written by hand. `throwError` constructs only the
`SessionUseError` branch, `catchError` inspects only that branch, and the other two flow past
every handler untouched. Uncatchability becomes a property of the `Monad` instance rather than of
a handler's care, which is the point: it is not a rule anyone can forget to follow.

Note what that does to `onConnectionState`'s guard. Once a `ConnectionUseError` is in the `Left`,
`>>=` short-circuits and no handler can clear it, so the rest of the session does not run - which
is precisely what the guard was refusing it one operation at a time. The guard is a short-circuit
written by hand because the type would not provide one; give the type the constructor and the
short-circuit is `>>=`.

Hand-writing four instances that were derived is the tax on having decided what they should mean.
The `via` clause is what produced the wrong meaning in the first place, so keeping it is not a
saving.

## What this deletes

The point of the change, stated as a list, because the argument above is easier to check against
one:

| Goes | Because |
|---|---|
| `ConnectionState.dead`, `setDead` | the verdict is a constructor |
| `onConnectionState`'s guard | `>>=` short-circuits on it |
| `Session.deadConnectionError` | there is no later operation left to refuse |
| `Errors.connectionIsSpent` | pattern matching answers it |
| `use`'s `either connectionIsSpent (const False)` net | `throwError` cannot mint a fatal error |

`deadConnectionError` going is the one with a user-visible edge to it. It is the anonymous error
that today replaces the real one for anyone who caught the real one, and half of what
[spent-connection errors name no cause](../problems/spent-connection-errors-name-no-cause.md)
is about. After the split the original failure is what propagates, since nothing intercepted it.
The other half of that document - `use` on a spent handle reporting "The connection is no longer
available" for both `release` and driver-finished routes - is untouched and stays open.

`Hasql.Comms.Roundtrip.toPipelineIO` stays as the merge left it: the mode exit is attempted on
every path except a failed send, and an exit failure is reported over the receive failure that
preceded it. The rule survives the split and its justification gets shorter. Today it reads "the
verdict is derived from the error alone, so reporting the receive failure would describe the
connection as healthy". After the split, reporting the receive failure would return a
`SessionUseError` - catchable, connection retained - on a connection still in pipeline mode,
which is issue #326 over again. Same rule, stated structurally instead of by reference to a
classifier.

## What `use` does on each path

| Outcome | `use` |
|---|---|
| `Right a` | hand the connection back |
| `Left (SessionUseError _)` | hand the connection back |
| `Left (ConnectionUseError _)` | finish the connection |
| `Left (DriverUseError _)` | finish the connection |
| exception | finish the connection, unchanged |

The exception row is where the merge already arrived, and the split does not touch it. An
exception lands somewhere inside a round trip and the driver has no way of finding out where,
while the repair it could attempt is blocking network IO performed under a mask, which on a
connection whose peer has gone away never returns - the interruption being handled would then
never land. Finishing the connection touches nothing but the socket. Note that this covers async
exceptions too, a `timeout` or a `killThread` landing mid-command, and `use` cannot tell those
from a `throwIO` the session made itself.

The two `Right`/`SessionUseError` rows are where the split does its work. Both mean the driver is
in control: a completed receive sequence, nothing in flight, nothing the driver both needs to and
can put right. After the split those are the only outcomes that can reach them, by construction
rather than by a flag check.

Two things are consequently the caller's responsibility and are not policed.

A transaction left open across a caught error is one. Whoever composed the transaction closes it.
`use` returns `Right` on a connection sitting in `TransInError` and nothing distinguishes that
from a clean return, so [hasql-pool#35](https://github.com/nikita-volkov/hasql-pool/issues/35)
stays open for callers who catch inside a hand-rolled transaction, and a pool that wants to act
on it reads the status through `onLibpqConnection`.

A pipeline left open by `onLibpqConnection` is the other. Such a connection refuses serial
commands and hands the next pipeline the stale results of this one, and nothing repairs it -
which is already the contract, pinned by `DirtyReturnCleanupSpec`. The split changes only the
vocabulary the escape hatch reports in.

## The state a caught error keeps

A caught error retains every connection-state change made before it. The newtype returns the
state on both branches, which is `ExceptT e (State s)` and not `StateT s (Either e)`, and the
difference is not academic: a statement whose `PARSE` succeeded and whose `EXECUTE` failed has
truthfully been prepared on the server, so discarding the cache entry would cost a `42P05` round
trip to rediscover it, and discarding resolved OIDs would cost a `SelectTypeInfo` each.

That retention carries an obligation, and the obligation is the half worth writing down, because
it constrains code that does not exist yet and is invisible from every call site: no operation
may record an effect it has not confirmed. `Session.statement` honours it by reverting the
statement cache except on a prepare collision, and `Pipeline.run` does the same across partial
progress. Both belong in the instance's haddock, alongside the statement of what `catchError`
cannot see, since a chosen meaning nobody wrote down is a slower kind of accident.

The state that survives a caught error is smaller after the split, since `dead` is no longer part
of it. What is left is caches, and caches are monotonic and connection-scoped.

## What stays and why

`MonadIO` stays as it is. `MonadError` was worth attacking because it misrepresented itself,
presenting recovery as safe where it was not. `liftIO` claims arbitrary IO happens and that is
what happens. Removing it would cost logging and metrics inside a session and close nothing,
since `onLibpqConnection` has to stay regardless.

`MonadError` stays a class rather than becoming a pair of plain functions. The reasons it looked
questionable were all consequences of it being wrong-typed. As a class it lifts through user
transformer stacks for free, so `ReaderT AppEnv Session` inherits catching, and `tryError`,
`handleError` and `liftEither` work off the shelf. Plain functions cannot reproduce that.

`throwError` stays, with no warning attached. On the flat type a minted error was a claim about
connection health that the driver's own machinery would act on - which is why `use` needs its
second net today. Narrowed, every value a user can mint asserts what happened and never what
state anything is in, so no false premise is left for anything to act on, and the net goes.
`Hasql.Errors` exports `SessionError (..)` anyway, because pattern matching is the type's whole
purpose, so removing `throwError` would close nothing while costing a rethrow that transaction
libraries need in order to roll back and re-raise. Application-level failure conditions still
belong in `ExceptT MyError Session`, where they stay the application's, but that is a
recommendation and not a caution.

## Why not stop at the flag

Because the flag is a workaround for the type, and ADR 0001 says so in its own words: the
derivation is "what we would still prefer", and the flag exists because `MonadError` blocked it.
Leaving it in place means keeping five pieces of machinery whose only job is to be right when a
handler is wrong, keeping an anonymous error that replaces the real one, keeping a classifier
that can drift from the constructors it classifies, and keeping the invitation that made
`hasql-transaction` mask its own errors.

The counter-argument is that the flag works and the split is a breaking change. It is a real
counter-argument, and the answer is that the breakage is the deliverable. A handler matching on
`ConnectionSessionError` inside a session is unreachable by construction after the split, and its
author is better served by a type error than by a handler that silently never fires.

## Rejected alternatives

1. **Cut at `ServerError`.** Narrow the instance to `MonadError ServerError`, so only
   server-reported failures are catchable. It excludes the fatal set correctly and excludes too
   much else with it: `UnexpectedRowCountStatementError`, row and cell decoding failures,
   `UnexpectedColumnTypeStatementError` and `MissingTypesSessionError` all leave the connection
   idle and healthy. Cutting on provenance answers nothing when a user asks why a unique
   violation is catchable and a null in column 3 is not.
2. **A predicate on the flat type.** Keep `SessionError` as it is and add `isConnectionHealthy`
   beside `isTransient`, with the recovery path consulting it. That documents the invariant
   instead of enforcing it, and predicates drift from the constructors they classify. `isTransient`
   did exactly that, hard-coded to `False` on `ServerError` until changelog #325, and
   `connectionIsSpent` is the same shape of thing waiting to do the same.
3. **A parallel narrow type.** Keep `SessionError` flat for reporting and give the recovery path
   its own `RecoverableError` with a total injection back. Two types with overlapping constructor
   sets have to be kept in sync by hand forever.
4. **A private exception for fatal failures.** Let `SessionError` narrow, and have the driver
   throw fatal failures rather than return them, so `catchError` cannot see them because it cannot
   see exceptions. It works, and it puts driver-fatal failures in the same channel as async
   exceptions and user `throwIO`, so `use` has to sort them apart by exception type. That
   reintroduces the provenance sniffing the split exists to remove, and it moves an error the
   driver can describe precisely out of the value channel where the rest of hasql's errors live.
5. **Keeping a refused send catchable.** libpq reports the connection fine after refusing a
   request on its own merits, so health alone would leave it a `SessionError` and spare the caller
   a reconnect. Rejected because the rule then stops being a rule: every send site would carry a
   judgement about which refusals are benign, and the connection would be handed on with the
   driver having seen its send path behave in a way it does not model.
6. **Reporting the receive failure over the pipeline exit failure.** `toPipelineIO` has two errors
   to choose between on an unhappy path and reports the exit failure. Reporting the receive
   failure instead would usually be the more informative of the two - it is often an ordinary
   server error the caller is meant to catch. Rejected because after the split that error is a
   `SessionUseError`, so it would hand back a connection still in pipeline mode, which is #326
   over again. The graver verdict wins even when it is the less interesting one.
7. **Discarding state on a caught error.** `StateT s (Either e)` rather than `ExceptT e (State s)`.
   Every OID resolved and every statement prepared before the failure would be thrown away and
   re-fetched, and the caches are monotonic and connection-scoped, so there is nothing the
   rollback would be protecting.

## What breaks

Release 2.1, no shim, no compatibility aliases and no pattern synonyms. The constructor split is
the point of the change, and synonyms that let an old `case` keep compiling would preserve
precisely the code that has to be reconsidered.

| Site | Effect |
|---|---|
| `Integration.Sharing.Session.CatchErrorSpec` | survives, `script "absurd"` is a server error |
| Hstore and Citext `CREATE EXTENSION` probes | survive, server errors |
| `Integration.Sharing.PipelineSpec` catch cases | survive, server errors |
| `Integration.Sharing.Pipeline.Statement.SendFailureSpec` | constructor renames only |
| `Integration.Sharing.Connection.Use.InterruptionSpec` | constructor renames only |
| `Integration.Sharing.Connection.UseSpec` | constructor renames only |
| `Integration.Sharing.Session.OnLibpqConnection.DirtyReturnCleanupSpec` | retyped with `onLibpqConnection` |
| `Integration.Sharing.Session.RecoveredPipelineFailureSpec` | rewritten, see below |
| `Pure.ErrorsSpec` | the `ConnectionSessionError` cases move to `UseError` |
| `connection-state-tests` | `dead` goes with the field |
| hasql-transaction | rollback-and-rethrow keeps working, and stops masking fatal errors |
| hasql-pool | `UseError` in signatures, #35 unchanged |

`RecoveredPipelineFailureSpec` has to be rewritten again, and this time it shrinks. Its three
cases all begin `catchError (Session.pipeline failingPipeline) (const (pure ()))`, and after the
split that handler never fires: the failure is a `ConnectionUseError`, so the session ends there
and the remainder - the refused `script`, the refused statement, the swallow-and-succeed - has no
way of happening. What replaces it is the assertion that the handler does not fire and that `use`
returns the fatal error whole. ADR 0001 already names this spec as the test of whether the
blocker is still there, so its rewrite is the signal that it is not.

The receive-side classification still wants a test of its own and still does not have one: a
connection killed mid-receive, from another session or by terminating the backend, has to come
back as `ConnectionUseError` rather than as a row-count complaint. The mechanism landed with the
merge; the test did not.

hasql-transaction is unaffected in the sense that nothing it does stops working, and improves in
the sense described at the top: a body failing with a `SessionError` is caught, rolled back and
rethrown, and a body failing connection-fatally is no longer caught at all, so `tryTransaction`
no longer issues an `ABORT` that gets refused and no longer discards the real error in favour of
the refusal.

`docs/downstream/hasql-pool-tasks.md` is unaffected apart from constructor names. All four of its
tasks - discarding on the exception path, the `initSession` policy question, the stale prose, and
double-`finish` tolerance - are about `use` finishing connections, which the merge already did and
this split does not revisit.

Implement in this order: split the error types first and let the compiler enumerate the call
sites, retype `onLibpqConnection` with them, then hand-write the instances against the split, and
delete the flag machinery last, when nothing catchable can reach it any more.
