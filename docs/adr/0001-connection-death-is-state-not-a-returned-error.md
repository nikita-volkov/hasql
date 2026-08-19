# Connection death is state, not a returned error

> **Status: Superseded by [ADR 0002](0002-narrowing-the-error-a-session-can-catch.md)**
>
> ADR 0002 implements the unblocking change described in the "What would unblock it" section
> below: the error type is split so fatal cases travel on a channel `catchError` does not
> cover. As a result, `ConnectionState.dead`, `onConnectionState`, and the per-operation
> guard have been removed. The analysis that follows is preserved for historical context.

---

When the driver gives up on a connection - a failed send, a socket lost mid-receive - it
records that verdict in `ConnectionState.dead` at the point of failure, rather than
letting `Hasql.Connection.use` derive it from the `SessionError` the session returns.
`use` finishes the connection when the flag comes back set, and every session operation
short-circuits on it.

`use` also derives the verdict from the returned error, as a second net beside the flag:

```haskell
if ConnectionState.dead newState || either connectionIsSpent (const False) result
```

The two catch different things. The flag catches a verdict the session went on to
swallow, which no derivation can see. The derivation catches an error that never passed
through `onConnectionState` and so never reached the flag - a session calling `throwError`
with a `ConnectionSessionError` of its own, for one. Neither subsumes the other, so both
run. Everything below is about why the flag cannot be dropped in favour of the derivation
alone.

The simpler design is to derive it: `use` finishes the connection whenever the session
returns `ConnectionSessionError` or `DriverSessionError`, with no flag and no per-operation
guard. That design is what we settled on first, and it is what we would still prefer. It
is blocked by `Session`'s `MonadError` instance, not by anything about connections.

## Why the simpler design does not hold

`Session` is `ConnectionState -> IO (Either SessionError a, ConnectionState)` with
`MonadError SessionError` derived through `ExceptT SessionError (StateT ConnectionState IO)`.
So a returned error is a *report*, and a report is catchable and discardable:

- A session can catch the failure and carry on. The rest of it then runs against a
  connection whose protocol state the driver has disowned - and a serial command issued
  while the connection is still in pipeline mode does not fail, it blocks forever waiting
  on results the server was never asked for, uninterruptibly.
- A session can catch the failure and succeed. `use` receives `Right`, the classification
  never runs, and the connection goes back to the pool poisoned.

Connection death is a fact about the resource, not a claim the session is making. Facts
about the resource have to live where the resource lives, which is the state threaded
through the session - the one channel `catchError` cannot intercept, drop, or rewrite.

This is the same argument that moved pipeline-mode restoration out of `use` and into
`Hasql.Comms.Roundtrip.toPipelineIO` before that restoration was deleted altogether: any
obligation discharged after the session returns is an obligation the session can arrange
never to trigger.

## What would unblock it

Any change that stops a connection-fatal failure from being catchable as an ordinary
session error. Dropping `MonadError` from `Session`; splitting the error type so the
fatal cases travel on a channel `catchError` does not cover; or making `catchError`
rethrow rather than absorb that subset. With any of those in place,
`ConnectionState.dead`, `Hasql.Engine.Contexts.Session.onConnectionState` and its guard
can go, and the derivation `use` already performs is left standing on its own.

Note that the classifier survives either way: `Hasql.Engine.Errors.connectionIsSpent` is
what sets the flag, and it is what `use` already consults directly. Only the flag's
placement is at stake, not the classification itself.

`Integration.Sharing.Session.RecoveredPipelineFailureSpec` is the test of whether the
blocker is still there. It catches a pipeline send failure inside a session and asserts
that the rest of the session is refused and the connection is finished anyway. Under the
simpler design its last case - the session swallowing the failure and succeeding - is the
one that breaks.
