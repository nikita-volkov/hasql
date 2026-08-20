# `AcquireError` is organized by stage, not by inferred cause

**Status: contested by [ADR 0005](0005-acquireerror-is-a-flat-set-of-observed-failures.md), which
accepts this document's argument about which facts may be published and rejects the shape derived
from it. Both are live; exactly one will remain so.**

`AcquireError`'s `NetworkingAcquireError`, `AuthenticationAcquireError` and `OtherAcquireError`
constructors are deleted, along with the two substring lists and the `interpretConnectionError`
function that fed them. What replaces them is a decomposition of `Hasql.Connection.acquire`'s own
control flow: one constructor per stage of acquisition, each carrying whatever the driver actually
observed at that stage.

```haskell
data AcquireError
  = ConnectionAcquireError Bool Text
  | CompatibilityAcquireError Text
  | InitializationAcquireError (Either Text ServerError)
```

[ADR 0003](0003-the-fatal-side-of-useerror-needs-no-subdivision.md) settles the negative half of the
rule this document completes: a driver should not publish a distinction it is not positioned to
observe. `isTransient` was that mistake as a method. The networking-versus-authentication split was
the same mistake as a constructor set, arrived at from a different direction, and surviving 0003
only because 0003 was about `UseError`. The positive half - which distinctions a driver *is*
positioned to publish, and why stage is one of them - is what this document adds.

## The classifier was reading prose

`interpretConnectionError` took `libpq`'s connection error message, lowercased it, and tested it
against fourteen networking substrings and four authentication substrings. First match won;
everything else fell through to `OtherAcquireError`. The constructor a caller received was a function
of English text.

Three separate things make that text an unsound thing to branch on, and they are independent of each
other, so fixing any one leaves the other two:

`libpq` translates its own messages. The strings are run through `gettext` against the client's
locale, so `LC_ALL=fr_FR.UTF-8` turns "could not translate host name" into something that matches
none of the patterns. An identical failure - identical host, identical network, identical server -
classifies as `NetworkingAcquireError` on one machine and `OtherAcquireError` on another, decided by
an environment variable the driver never reads and the caller may not know is set. This was reported
as #329, and the fix that shipped for it added more patterns, which does not touch the mechanism.

The message is not part of the adapter contract. `Pqi.errorMessage`'s own haddock says the flat text
"is formatted locally by the driver, so adapters are not expected to produce byte-identical
strings". Hasql runs over `pqi-ffi` and `pqi-native`, and the classifier's output therefore depends
on which one is linked. That the integration tests pass today is a statement about two
implementations happening to agree on a phrase, not about anything either of them promised.

No structured signal exists to fall back on. A failed `PQconnectdb` produces no `PGresult`, so there
is no `PQresultErrorField` to read and no SQLSTATE to branch on. This is the crucial asymmetry: for
a failure *during a session* the driver can classify honestly, because the server sends a structured
error report. For a failure *while connecting* it cannot, because the server either never spoke or
spoke only far enough to say no. The old constructor set papered over that asymmetry by presenting
guesses in the same shape as facts.

## Stage is a fact the driver observes

Deleting the guesses does not force the type down to a single opaque constructor, because there is a
distinction available that costs nothing to obtain and is not an inference at all.

`acquire` runs three stages in sequence: it establishes a connection, it checks the server version,
and it initializes session settings. Which stage a failure came from is known by control flow - the
driver is standing at the failure site - and is therefore locale-independent, adapter-independent,
and not derived from any message. It is the same kind of knowledge as
`CompatibilityAcquireError`'s, which was always the one honest constructor in the old set: it
reports a comparison Hasql performed itself.

Stage is also more informative to a caller than the classification it replaces, because reaching a
stage is a claim about everything before it. `InitializationAcquireError` says: DNS resolved, the
socket opened, TLS negotiated if requested, the credentials were accepted, the database exists, and
the server is at least version 9. Every one of those is ruled out as the cause. The old
`OtherAcquireError` - where session-initialization failures landed, since no substring matched -
said none of that. It was the constructor for "no pattern matched", which is a fact about the
pattern list rather than about the connection.

`ConnectionAcquireError` correspondingly lumps together DNS failure, connection refused, TLS
negotiation failure, a rejected password, a missing database and "sorry, too many clients already".
That range looks like a regression from a set of constructors that named several of those
separately. It is not: the old constructors named them without being able to tell them apart, and
the set of things they named was not even a partition - "the database system is starting up" was
filed under networking on the grounds that it is transient, which is the retryability verdict 0003
declines to issue, reappearing as a pattern-list entry. One constructor that says "the connection
could not be established, here is what libpq said" claims exactly as much as the driver knows.

## The one structured signal at the connection stage

`PQconnectionNeedsPassword` reports whether the server demanded a password and none was available.
It is a flag `libpq` sets from the authentication exchange, not from message text, and it survives
the failed connection for exactly this kind of interrogation. It is the single fact about a failed
`PQconnectdb` that meets the standard the rest of this document applies, so it is kept, as the `Bool`
on `ConnectionAcquireError`.

It is deliberately *not* promoted to its own constructor. The constructors of `AcquireError` are
stages, and a missing password is not a stage - it is a refinement of one. A sibling constructor
would put a sub-case of the connection stage at the same level as the stages themselves, so the type
would be organized by stage except for one member, and a caller reading the constructor list would
have no way to tell which principle was in force. A field on the stage it belongs to keeps the
decomposition total and the axis single.

Its sibling `PQconnectionUsedPassword` is not used, and the distinction is worth recording because
the two flags look interchangeable. `usedPassword` reports that a password was *sent*, which stays
true when authentication succeeded and the connection then failed for an unrelated reason - a
missing database, say. `usedPassword && failed` therefore does not imply an authentication failure,
and a constructor derived from it would be wrong in exactly the cases a caller would most want it to
be right. `needsPassword` carries no such ambiguity: it is only ever set when the exchange could not
be completed.

Both facts are `libpq`'s semantics, and Hasql relies on `libpq`'s semantics. An adapter that reports
something else is diverging from the contract it implements, which is a bug in the adapter and not
an input to this design.

## The initialization stage has two knowledge levels

Session initialization runs `SET client_encoding` and `SET client_min_messages` through `PQexec`,
and it can fail in two structurally different ways.

The connection can die, in which case there is prose and nothing else. This covers `PQexec`
returning null, and - the more common shape - `PQexec` returning a `PGresult` that `libpq`
fabricated client-side with `PGRES_FATAL_ERROR` and no diagnostic fields on it. Both are the same
event, and the seam between them is not where it appears to be: "did `exec` return a result" is not
the question, "does the result carry a SQLSTATE" is.

Or the server can reject the statement and say why, in which case there is a real error report with
`PG_DIAG_SQLSTATE`, `PG_DIAG_MESSAGE_PRIMARY` and the rest, readable through `PQresultErrorField`.
`Either Text ServerError` is that distinction: `Left` for the connection-died case, `Right` for a
server that answered. It makes `InitializationAcquireError` the first and only `AcquireError` for
which `toSqlState` can return `Just`, which the old code discarded by flattening the report into a
`Text` reason.

## No residual constructor

The three constructors partition `acquire`'s failure modes exhaustively, so there is nothing left
for a catch-all to hold, and `OtherAcquireError` is deleted rather than retained as a safety valve.

That is worth doing on purpose rather than by omission. A residual constructor that nothing
constructs is not inert: it is where the next stage added to `acquire` will land if nobody thinks
about it, and a reviewer has no way to notice, because the catch-all was always going to be reached
by something. Without one, adding a stage forces a constructor and a decision about what that stage
observes. "There is no uncategorized case" is a stronger statement about a driver's error type than
any documentation, and it is only true while nothing can quietly make it false.

## Rejected alternatives

1. **Keep the constructors, fix the classifier.** Match on more patterns, or normalize the locale by
   setting `LC_MESSAGES=C` on the connection before classifying. Rejected because the locale is only
   one of the three independent reasons the text is unsound, and forcing it is not the driver's call
   to make: `libpq` reads the process environment, so a driver that overrode it would be changing
   the language of messages the application also shows its own users.
2. **Derive an authentication constructor from `connectionNeedsPassword || connectionUsedPassword`.**
   Rejected on `usedPassword`, for the reason above: it is true in cases that are not authentication
   failures, so the constructor would misreport precisely the failures that matter. The half of it
   that is sound is kept as a field.
3. **Drop `needsPassword` too, leaving `ConnectionAcquireError Text`.** The most minimal reading of
   "publish only what you observe". Rejected because `needsPassword` *is* observed - it is a flag,
   not an inference - and the rule is about the provenance of a distinction, not about minimizing
   the count. Discarding it would throw away the one signal that survives the argument this document
   makes.
4. **A separate `MissingPasswordAcquireError` constructor.** More convenient at the match site than
   a positional `Bool`. Rejected for mixing axes: see above. The convenience is real and the cost is
   a type that no longer explains its own shape.
5. **A nested sum, `ConnectionAcquireError ConnectionFailure`, to keep the axis pure while naming
   the case.** Rejected as buying axis purity with a second exported type and a two-level match, for
   a sum with exactly two members, one of which is "nothing further is known".
6. **Two constructors for the initialization stage instead of `Either Text ServerError`.** Rejected
   because the two halves are one stage at two knowledge levels, not two stages, and the same
   argument that keeps missing-password off the top level keeps these off it.
7. **Name the stages after `libpq`'s protocol phases - `HandshakeAcquireError`.** Rejected because
   the first stage covers DNS resolution and TCP connection as well, which precede any handshake;
   `could not translate host name` is not a handshake failure. `ConnectionAcquireError` names what
   the stage is for.

## What breaks

`AcquireError` has not shipped in a release - it exists only in the `Upcoming` section of the
changelog, introduced there alongside `UseError`. As with ADR 0003, this is a revision of unreleased
work, so the changelog entry is rewritten to describe the end state rather than layering a second
breaking change over the first.

| Site | Effect |
|---|---|
| `Hasql.Engine.Errors.AcquireError` | `NetworkingAcquireError`, `AuthenticationAcquireError` and `OtherAcquireError` removed; `ConnectionAcquireError` and `InitializationAcquireError` added; `CompatibilityAcquireError` unchanged |
| `Hasql.Connection.acquire` | `interpretConnectionError` and both substring lists deleted; the status-check site reads `connectionNeedsPassword`; the initialization site reads `resultErrorField` to build a `ServerError` |
| `Hasql.Errors.IsError` | the `AcquireError` instance overrides `toSqlState`, delegating to the `Right` of an `InitializationAcquireError` |
| `Integration.Isolated.Connection.Acquire.ErrorClassificationSpec` | deleted whole: both its tests target the classifier, and its apparatus - a re-exec of the test binary under `LC_ALL=fr_FR.UTF-8`, and a bespoke `docker run` with `max_connections=1` - existed only to exercise locale sensitivity and the pattern list |
| `Integration.Isolated.Connection.AcquireSpec` | the `AuthenticationAcquireError` cases become the two live-server tests that pin `needsPassword`'s semantics: no password against a password-demanding server reports `True`, a *wrong* password reports `False`; the `NetworkingAcquireError` cases assert `ConnectionAcquireError` with `False` |
| `Pure.Connection.AcquireSpec` | one test per structural path, through the existing fake adapter; `connectionNeedsPassword` stops being `unimplementedIO` on `fakeConnection` |
| `Pure.ErrorsSpec` | rendering assertions for the removed constructors replaced |
| `pqi-native` | its `connectionNeedsPassword` returning a constant is a divergence from `libpq`, fixed upstream rather than accommodated here |
