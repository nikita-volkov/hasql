# Nothing can say "it succeeded and the connection is gone"

Status: open. Raised against 6c9f27b7, which had to choose a side and chose one.
Not a claim that the choice was wrong - a claim that the choice exists.

A round trip has two outcomes to report and one slot to report them in. Whether
the operation succeeded, and whether the connection survived it, are
independent, and `Recv` returns `Either (Error tag) a`. So when they disagree,
something is dropped.

## Where it bites

`Hasql.Comms.Recv.singleResult` reads the result, then reads again expecting the
`Nothing` that ends the command, then decodes. 6c9f27b7 made that second read
consult `PQstatus`:

```haskell
  ExceptT do
    result <- Pq.getResult connection
    case result of
      Nothing -> do
        status <- Pq.status connection
        if status == Pq.ConnectionBad
          then do
            errorMessage <- Pq.errorMessage connection
            pure (Left (NoResultsError tag True errorMessage))
          else pure (Right result)
```

By that point the result is in hand. libpq hands back a `PGresult` as soon as it
has parsed `CommandComplete`; `ReadyForQuery` may still be in flight. If the
socket dies in between, the command ran and committed - autocommit, one
statement - and this branch discards its result and reports
`ConnectionSessionError` instead.

`allResults` gained the same check on its loop terminator, so a script reports
the same way, having decoded every statement in it successfully.

`ConnectionSessionError` is transient. A retry wrapper re-runs the operation. For
a `select` that costs a round trip. For an `insert` it writes twice.

## Why the commit is still right about the case it targets

The first read is a different matter. `Nothing` there means no result ever
arrived, and without the status check that is reported as
`UnexpectedRowCountStatementError` - "expected 1 row, got 0" - which is not
transient, so retry wrappers do not retry it and pools return the dead
connection for reuse. That is the bug the branch set out to fix, and 6c9f27b7
extends the fix to the terminations that were missed.

The disagreement is only about the terminations that follow a result the driver
already holds. There, "the connection is bad" and "the operation failed" are not
the same sentence, and the type makes them one.

## The channel already exists, one layer up

`ConnectionState.dead` is exactly the "the connection is gone" channel, and it is
independent of what the operation returns - that independence is the argument of
[ADR 0001](../adr/0001-connection-death-is-state-not-a-returned-error.md).
`Hasql.Comms` cannot reach it, being below `ConnectionState` entirely. But
`Hasql.Engine.Contexts.Session.onConnectionState` can, and it already inspects
every operation's outcome:

```haskell
      (result, newConnectionState) <- f connectionState
      pure
        ( result,
          case result of
            Left err | Errors.connectionIsSpent err -> ConnectionState.setDead newConnectionState
            _ -> newConnectionState
        )
```

A success arm that consults `Pq.status` and sets `dead` on `ConnectionBad` would
report the result the server produced *and* spend the connection: the caller
keeps its rows, `use` finishes the connection, the pool discards it, and nothing
gets written twice. The post-result status checks in `Recv` could then go back to
returning `Right`.

The cost is one `PQstatus` per session operation on the success path. It is a
local read of the connection struct, no syscall, so it is the same order of cost
as the checks 6c9f27b7 already added on the failing paths.

## What would settle it

Whether the split-frame window is real enough to pay for. `CommandComplete` and
`ReadyForQuery` almost always arrive in one segment, and a connection that dies
between them is narrow. Narrow is not never, and the failure it produces is a
duplicated write reported as a retryable error, which is the kind that surfaces
as data rather than as an exception.

Deliberately not implemented here. It touches a just-pushed commit's reasoning
and it puts a status call on the hot path, which is the author's call to make.
