# Spent-connection errors name no cause

Status: open. Stated here, not solved. Nothing in the driver is waiting on it.

Once the driver gives up on a connection, everything it says about that
connection afterwards is generic. Two errors carry the verdict, and neither one
carries the failure that produced it.

## The two errors

`Hasql.Engine.Contexts.Session.deadConnectionError`, returned by every session
operation attempted after the verdict was recorded:

```
ConnectionSessionError "The driver has given up on this connection earlier in the session"
```

`Hasql.Connection.use` on a handle whose connection is gone:

```
ConnectionSessionError "The connection is no longer available"
```

The second is reached by two routes that have nothing in common:
`Hasql.Connection.release`, and `use` having finished the connection itself
after a failed send, a socket lost mid-receive, or an exception cutting the
session short. The message covers both because it names neither.

That is an improvement on where this started. The message used to read "The
connection has been released", which was accurate for one route and a
description of something the caller never did for the other; 13915f63 replaced
it. What is left is not wrong, only silent: a caller looking at a spent handle
cannot tell whether they released it or whether the driver spent it, and if the
driver spent it, on what.

## Where it lands

Both errors are what a caller sees when a connection dies, so both surface
outside the driver.

`hasql-pool` returns them verbatim as `SessionUsageError`. A pool user whose
connection died mid-send is told the connection is no longer available, which is
what they would also be told had the pool been released underneath them - a
different situation with a different fix, and nothing in the message separates
the two.

`hasql-transaction` amplifies the first one. `tryTransaction` catches an error
from the transaction body and issues `ABORT` before classifying it:

```haskell
bodyRes <- catchError (fmap Just body) $ \error -> do
    statement () Statements.abortTransaction
    handleTransactionError error retryOnError $ return Nothing
```

When the body error is connection-fatal, the driver refuses the `ABORT` and the
handler fails on it, so `handleTransactionError` never runs and the original
`error` is discarded. The caller gets the refusal instead. Before the connection
was spent at the point of failure, the refusal at least carried libpq's own
message; now it carries the sentence above and nothing else. The masking is
`hasql-transaction`'s to fix, but what the mask leaves behind is the driver's.

## Why it is not just a wording fix

Both messages are anonymous because neither carrier holds a cause to name.

`ConnectionState.dead` is a `Bool`. Making it `Maybe SessionError` so
`deadConnectionError` can quote what happened is mechanical, but it changes what
the flag is. Today the flag is a verdict about the resource, deliberately
separate from the error a session reports - that separation is the whole
argument of [ADR 0001](../adr/0001-connection-death-is-state-not-a-returned-error.md).
Carrying an error inside it puts a report back into the resource's state and
invites the two to be read as one thing again.

The duplication is real, too. The error that killed the connection was already
returned to the session once, at the operation that failed. A session that
caught it has it in hand. Quoting it again in every subsequent refusal is
repetition for the caller that swallowed it and noise for the caller that
didn't.

`Connection` is `MVar (Maybe ConnectionState)`, which has exactly two states and
no room for why the connection is absent. Telling "you released this" from "the
driver finished this after X" means widening it - `MVar (Either Gone
ConnectionState)` with a `Gone` that names the route, and every site that
matches on `Nothing` today learning about the new shape. That is a state-machine
change bought entirely with message quality.

## What would settle it

Whether a caller can act on the difference. If knowing *why* a connection is
gone changes what a pool or a retry wrapper does, the cause belongs in the type
and the widening pays for itself. If it only ever reaches a log line, the
present anonymity is the cheaper trade and this document is the record of having
chosen it.

Worth noting either way: the classification is unaffected. Both messages are
`ConnectionSessionError`, both are transient, and pools discard on them today
regardless of what they say.
