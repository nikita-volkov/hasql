# The fatal side of `UseError` needs no subdivision

`UseError`'s two fatal constructors, `ConnectionUseError` and `DriverUseError`, merge into one.
`isTransient` is deleted from `IsError`, on every instance, with no replacement. Both changes
share one root cause, so they are argued together rather than as two separate decisions.

[ADR 0002](0002-narrowing-the-error-a-session-can-catch.md) settles the axis that matters:
`SessionError` is for failures that leave the connection usable, and everything else is fatal and
structurally uncatchable. That axis is untouched here. What this document revisits is whether the
fatal side needed a second axis underneath it - and whether `isTransient` was ever a question the
driver was positioned to answer.

## The boolean that lied at its own boundary

`isTransient` asked "will retrying against a clean connection state succeed?" `ConnectionUseError`
answered `True`. That answer is correct about the operation and false about every handle that could
receive it: `Hasql.Connection.use` finishes the connection before returning `ConnectionUseError`, so
the `Connection` value the caller is holding never carries a clean state again. Every later `use` on
that same handle reports the same "transient" error. A retry loop that branches on `isTransient` and
reuses the handle does not eventually recover; it spins forever on an error that is permanently
transient.

This is not a corner case reachable only by misuse. It is the modal case. Every fatal error `use`
returns is connection-fatal by definition - that is what makes it fatal - so `isTransient` returning
`True` on `ConnectionUseError` was always describing a connection the caller no longer has.

The library managed this by documentation instead of by fixing the type. The same caveat - reporting
`True` here does not mean what `True` means everywhere else, retry against a fresh connection, never
against this handle - was written three times: on `isTransient`'s class-level haddock, on
`ConnectionUseError`'s own haddock, and again on the `UseError` instance. Three copies of a caveat
that exists because a method promises something the driver cannot honestly give at the one place it
matters most is not a documentation debt to pay down. It is a sign the method is asking the wrong
question of the wrong layer.

## Where `DriverUseError` came from, and why it stops being needed

Issue #327 arrived from the other side of the same defect. `libpq` refuses a request outright when it
receives more than 65535 parameters, without the socket going bad. Under the two-constructor design
that predates this document, "connection lost" and "request refused, connection fine" collapsed into
the single `ConnectionUseError`, so a refused request was reported `isTransient = True`. A retry
wrapper resent the identical rejected request forever, because nothing told it the failure would
recur on any connection.

`DriverUseError` was added to fix that: a second fatal constructor whose only job was to report
`isTransient = False`. It worked, in the narrow sense that the boolean became accurate for that one
case. But it worked by teaching `UseError` a second axis - retryability - layered under the axis ADR
0002 already established (catchable vs. fatal), and every fatal outcome now had to be sorted onto it.
`fromSendError` classified by `Pq.status`; `fromRecvError` and `fromRecvErrorInScript` classified by
whether a tag was present, defaulting to "bug in Hasql or the server misbehaving" when it was not. The
constructor a caller received depended on machinery that existed for no purpose but to compute a
boolean that was already wrong on the more common constructor.

Once `isTransient` is gone, `DriverUseError` has nothing left to report that `ConnectionUseError`
does not already say: the connection is gone, discard the handle. The reason text - which already
distinguished a dropped socket from a refused request from a Hasql bug - is what carried the
diagnostic value all along, and it survives untouched on the merged constructor. Issue #327's actual
defect was never "the type has too few constructors"; it was that `isTransient` promised an answer
the driver was not positioned to give. Removing the boolean fixes it more directly than adding a
constructor did, which is why the constructor can now be reverted.

## Retry classification is not the driver's job

The deeper claim, and the reason no replacement is provided: a driver-level rendering interface is
the wrong place to decide what is worth retrying.

Retryability is a property of the caller's situation, not of the error value in isolation. Whether a
serialization failure is worth retrying depends on whether the caller can safely re-run its
transaction, which the driver has no visibility into. Whether a connection-fatal error is worth
retrying depends on whether the caller has a pool to draw a fresh connection from, which is
configuration the driver does not hold. A method on `IsError` that tries to fold both into one
boolean is answering a question that belongs one or two layers up, and the 42P05 case already showed
what happens when the driver tries anyway: correct today, and wrong the moment PostgreSQL's own
classification of that SQLSTATE shifts under it, or the moment a caller's actual constraints differ
from what the driver assumed.

What Hasql keeps is what it can honestly provide: a rendered message, structured details, and - where
the error carries one - the SQLSTATE, through `toSqlState`. A caller who wants to build a retry policy
has the constructor (`ConnectionUseError` means the handle is spent, full stop) and the SQLSTATE
(checked against the PostgreSQL error-code appendix, with whatever local knowledge the caller has
about their own transactions and topology). Both are honest primitives. `isTransient` was a
conclusion dressed as a primitive, and the conclusion was wrong at the constructor that matters most.

## What survives the deletion

The 42P05 SQLSTATE table in the `ServerError` instance is deleted along with `isTransient` - it
existed only to feed the method. Its reasoning does not disappear with it. `isPrepareCollision` in
`Hasql.Engine.Errors` already carries the operational fact that matters: a `Parse` failing with
`42P05` is safe to treat as a cache hit, not because PostgreSQL's error-code appendix says the
SQLSTATE is retryable, but because Hasql content-addresses prepared statement names as a digest of
the SQL text and parameter OIDs, so a name collision is a statement collision. That is a fact about
Hasql's own naming scheme, and it now lives on the one function that consumes it, rather than as a
line in a table whose only reader was a method being deleted.

`Recv.NoResultsError`'s internal `Bool` flag is not touched by this change, and deliberately so. It is
easy to look at `Roundtrip.ClientError`'s flag disappearing and assume its sibling should go the same
way; it should not, because the two flags answer different questions. `ClientError`'s flag chose
between two fatal outcomes - exactly the distinction this document argues has no reader left.
`NoResultsError`'s flag chooses between fatal and recoverable: whether the socket is gone
(`ConnectionUseError`) or the connection is intact and a statement genuinely returned no rows
(`SessionUseError` carrying a row-count mismatch). That is the same axis ADR 0002 already
established, applied at the point where a dropped connection and an empty result set would otherwise
be indistinguishable. Deleting it would resurrect the defect #327's predecessor already fixed once -
a dropped socket reported as a statement that returned nothing - so it stays, and its haddock now says
so, to survive being deleted later by analogy with the flag that did go.

## Issue #325

#325 reported that `isTransient` was hard-coded to `False` on `ServerError` and did not consult the
SQLSTATE, so serialization failures, deadlocks, lock timeouts and several other genuinely retryable
codes were misclassified. The report was correct. The fix that shipped for it - consulting a SQLSTATE
table - was also correct as far as it went, but it was a fix to the wrong layer: it made the driver
better at a job this document now declines to do at all.

#325 is closed as deliberately reverted, not silently dropped. The position is recorded on the issue
itself: Hasql renders errors and exposes `toSqlState`; it does not issue a retryability verdict. A
caller who wants the classification #325 asked for can build it from `toSqlState` against their own
policy, which is more honest than a table the driver would otherwise have to keep in sync with
PostgreSQL's error-code appendix forever. The next request shaped like #325 has this document to point
at.

## Rejected alternatives

1. **Keep `isTransient`, fix only the `ConnectionUseError` case.** Special-case the fatal
   constructors to report `False` while leaving `isTransient` in place for `ServerError` and its
   wrappers. Rejected because the method's contract - "retrying against a clean connection state will
   succeed" - is sound for those cases only by accident: it still says nothing about whether the
   caller's transaction can be safely re-run, which is the caller's fact, not the driver's. Patching
   the one boundary that was visibly wrong leaves the underlying category error in place.
2. **Replace `isTransient` with a free function over `toSqlState`.** Offer
   `isRetryableSqlState :: Text -> Bool` outside the class, so callers can opt in without the driver
   claiming a verdict as part of rendering. Rejected because it is the same table under a different
   name, with the same drift risk against PostgreSQL's own classification, and it still answers a
   question that depends on the caller's transaction semantics, which the table cannot see.
3. **Keep the three-constructor `UseError` and only delete `isTransient`.** Leave
   `ConnectionUseError` and `DriverUseError` as they were, dropping only the class method. Rejected
   because with the method gone, the two constructors carry no distinction any caller was ever shown
   to consume - `hasql-pool`'s discard predicate already treats both as discard-worthy - and every
   site that builds a `UseError` would still have to classify a failure onto an axis nothing reads.
   The constructor split earns its keep only if something downstream branches on it; nothing does.
4. **An `Either SessionError Text` in place of the named `UseError`.** The merged two-constructor
   shape is isomorphic to `Either`. Rejected as it was rejected in ADR 0002: named constructors state
   the operational meaning at the call site - `ConnectionUseError` reads as "the connection is gone"
   without consulting which side of `Either` is which - and an `Either` does not buy anything back for
   giving that up.

## What breaks

None of `UseError`, `IsError`, or `isTransient` shipped in a release; all three exist only in the
`Upcoming` section of the changelog through the point this document lands. This is a revision of
unreleased work, not a second breaking change against a shipped API, so the changelog is rewritten to
describe the end state directly rather than layering a second entry over the first.

| Site | Effect |
|---|---|
| `Hasql.Errors.IsError` | `isTransient` removed from the class and every instance |
| `Hasql.Engine.Errors.UseError` | `DriverUseError` removed; its call sites now construct `ConnectionUseError` |
| `Hasql.Comms.Roundtrip.ClientError` | its `Bool` flag removed; `fromSendError` takes one argument |
| `Hasql.Comms.Recv.NoResultsError` | unchanged; its flag is not the one this document is about |
| `Pure.ErrorsSpec` | all transience assertions removed; `DriverUseError` rendering assertions removed |
| `Integration.Sharing.ErrorsSpec` | the 65535-parameter test asserts fatal-and-closed, not non-transient |
| `Integration.Sharing.Pipeline.Statement.SendFailureSpec` | `DriverUseError` matches become `ConnectionUseError`; the standalone transience test is removed |
| `Integration.Sharing.Session.RecoveredPipelineFailureSpec` | `DriverUseError` matches become `ConnectionUseError` |
| `Integration.Sharing.Connection.Use.InterruptionSpec` | the "stays transient forever" test is replaced by one asserting the handle keeps reporting itself gone |
| `hasql-pool` | untouched; it already treats both merged constructors as discard-worthy and never called `isTransient` |
