# `AcquireError` is a flat set of observed failures

**Status: accepted. Supersedes [ADR 0004](0004-acquireerror-is-organized-by-stage-not-by-inferred-cause.md),
whose argument about which facts may be published is adopted here in full and whose grouped shape is
rejected.**

`AcquireError`'s `NetworkingAcquireError`, `AuthenticationAcquireError` and `OtherAcquireError`
constructors are deleted, along with the two substring lists and the `interpretConnectionError`
function that fed them. What replaces them is five constructors on one level, each named for a
failure the driver actually observed:

```haskell
data AcquireError
  = ConnectionAcquireError Text
  | ConnectionPasswordRequiredAcquireError Text
  | VersionTooOldAcquireError Int Int Int
  | InitializationConnectionLossAcquireError Text
  | InitializationServerErrorAcquireError ServerError
```

[ADR 0003](0003-the-fatal-side-of-useerror-needs-no-subdivision.md) settles the negative half of the
rule this document completes: a driver should not publish a distinction it is not positioned to
observe. `isTransient` was that mistake as a method. The networking-versus-authentication split was
the same mistake as a constructor set, arrived at from a different direction, and surviving 0003
only because 0003 was about `UseError`. The positive half - which distinctions a driver *is*
positioned to publish, and what shape they should take once identified - is what this document adds.

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

## What survives as publishable

Deleting the guesses does not force the type down to a single opaque constructor. Four distinctions
survive the argument above, because the driver observes rather than infers them.

**Which stage of `acquire` failed.** `acquire` runs three stages in sequence: it establishes a
connection, it checks the server version, and it initializes session settings. Which stage a failure
came from is known by control flow - the driver is standing at the failure site - and is therefore
locale-independent, adapter-independent, and not derived from any message.

Stage is also more informative to a caller than the classification it replaces, because reaching a
stage is a claim about everything before it. A failure at initialization says: DNS resolved, the
socket opened, TLS negotiated if requested, the credentials were accepted, the database exists, and
the server is at least version 9. Every one of those is ruled out as the cause. The old
`OtherAcquireError` - where session-initialization failures landed, since no substring matched -
said none of that. It was the constructor for "no pattern matched", which is a fact about the
pattern list rather than about the connection.

**`PQconnectionNeedsPassword`.** Covered in its own section below.

**The server-version comparison**, which Hasql performs itself. This was always the one honest thing
in the old constructor set: `CompatibilityAcquireError` reported a comparison the driver made, not a
phrase it recognized.

**Whether an error report carries a SQLSTATE.** Covered in the initialization section below.

Nothing else does. In particular, the range now collapsed into `ConnectionAcquireError` - DNS
failure, connection refused, TLS negotiation failure, a rejected password, a missing database,
"sorry, too many clients already" - looks like a regression from a set of constructors that named
several of those separately. It is not: the old constructors named them without being able to tell
them apart, and the set of things they named was not even a partition. "The database system is
starting up" was filed under networking on the grounds that it is transient, which is the
retryability verdict 0003 declines to issue, reappearing as a pattern-list entry.

## The one structured signal at the connection stage

`PQconnectionNeedsPassword` reports whether the server demanded a password and none was available.
It is a flag `libpq` sets from the authentication exchange, not from message text, and it survives
the failed connection for exactly this kind of interrogation. It is the single fact about a failed
`PQconnectdb` that meets the standard the rest of this document applies, so it is kept, as
`ConnectionPasswordRequiredAcquireError`.

Its sibling `PQconnectionUsedPassword` is not used, and the distinction is worth recording because
the two flags look interchangeable. `usedPassword` reports that a password was *sent*, which stays
true when authentication succeeded and the connection then failed for an unrelated reason - a
missing database, say. `usedPassword && failed` therefore does not imply an authentication failure,
and a constructor derived from it would be wrong in exactly the cases a caller would most want it to
be right. `needsPassword` carries no such ambiguity: it is only ever set when the exchange could not
be completed. A *wrong* password, correspondingly, reports `False` and lands in
`ConnectionAcquireError` - the driver cannot tell it from any other refusal, and does not pretend to.

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
the question, "does the result carry a SQLSTATE" is. That is
`InitializationConnectionLossAcquireError`.

Or the server can reject the statement and say why, in which case there is a real error report with
`PG_DIAG_SQLSTATE`, `PG_DIAG_MESSAGE_PRIMARY` and the rest, readable through `PQresultErrorField`.
That is `InitializationServerErrorAcquireError`, and it is the first and only `AcquireError` for
which `toSqlState` can return `Just` - which the old code discarded by flattening the report into a
`Text` reason.

## Constructors, not fields

The facts above fix what the type may say. They do not by themselves fix its shape, and this is
where ADR 0004 overreached. Having established that stage is publishable and inferred cause is not,
0004 concluded that the constructors should therefore *be* stages, with every other surviving fact
demoted to a field:

```haskell
  = ConnectionAcquireError Bool Text
  | CompatibilityAcquireError Text
  | InitializationAcquireError (Either Text ServerError)
```

That step does not follow. The flat set publishes precisely the same facts - nothing is lost,
nothing is added. `ConnectionPasswordRequiredAcquireError` is 0004's `Bool`; the initialization pair
is 0004's `Either`. The two shapes are isomorphic, so the choice between them cannot be settled by
the observability argument, which both satisfy equally, and has to be settled on what the shape
costs a reader.

On that question the flat set wins on three counts and, as far as this document can determine, loses
on none.

**Named cases instead of a positional `Bool` and an unnamed `Either`.** `ConnectionAcquireError Bool
Text` requires haddock to explain what `True` means, at the definition and again wherever anyone
matches it. `Either Text ServerError` requires haddock to say which side is the connection-died
case. The flat constructors require neither. A type whose shape has to be annotated to be read is a
type that could have been shaped better.

**One match level.** Rendering an `AcquireError` under 0004 means three top-level alternatives, then
a `Bool` inspection inside one and an `Either` match inside another. Under the flat set it is five
alternatives and no inner matching. The exhaustiveness checker covers all five directly, rather than
covering three and leaving the inner two shapes to be handled by hand.

**The nesting has nothing to nest.** Hierarchy earns its keep when many cases share a level and the
grouping saves a reader from reading them all. Here the connection stage holds two cases and the
initialization stage holds two. A two-level structure over four leaves is the weakest instance of
the pattern there is, and the stage information is not lost by flattening - it moves into the
constructor names.

## The one defense of nesting, and why it is disqualified

The strongest argument for the grouped shape is that it lets a caller handle a whole stage at once:
match `ConnectionAcquireError` and treat every way of failing to connect uniformly, without
enumerating them.

That convenience is real, and it is a retry-grouping convenience - the caller who wants it wants it
because connection-stage failures are the ones worth another attempt against a pool. ADR 0003
settles that this is not a consideration Hasql designs around: retryability depends on the caller's
policy and topology, which the driver cannot see, and a driver that shapes its error type to make
one particular retry partition convenient has issued the verdict 0003 declines to issue, in a form
that is harder to notice than a boolean was. A caller who wants that partition can write it;
enumerating two constructors is not a hardship, and writing it themselves keeps the policy where it
belongs.

Nor does nesting buy anything on evolution. The apparent advantage - a new `libpq` flag enriches the
`Bool` into a sum without disturbing the top level - is not an advantage, because widening a
positional `Bool` into a sum type is exactly as breaking for callers as adding a constructor. Both
shapes cost the same to iterate, which is what the type should expect to do as the observable facts
change.

## Naming: stage prefix, then case

Flattening moves the stage information into the names, so the names carry a rule:

**A constructor is named for its stage, then its case. A bare stage name means nothing further is
known at that stage. A stage with only one case needs no prefix.**

So `ConnectionAcquireError` and `ConnectionPasswordRequiredAcquireError` read as siblings at the
connection stage, with the bare one visibly the residual rather than a vague peer of a specific one.
`InitializationConnectionLossAcquireError` and `InitializationServerErrorAcquireError` do the same
at the initialization stage, and their prefix is the most valuable thing they say: everything before
initialization succeeded. `VersionTooOldAcquireError` carries no prefix because its stage has
exactly one case and there is nothing to group it with - a prefix earns its keep only where a stage
has siblings.

The rule also does the work an `UndiagnosedConnectionAcquireError` would have done, at lower cost.
The hazard it guards against is a future maintainer of `acquire` reaching for the plain-sounding
constructor when the password case applies; making the plain name *mean* "residual within this
stage" by convention addresses that without paying for a long word borrowed from `libpq`'s
`PG_DIAG_*` vocabulary - vocabulary that is doubly odd here, since the constructor exists precisely
because no diagnostic fields are produced for a failed `PQconnectdb`.

`CompatibilityAcquireError` does not survive the rule and is renamed. It is vague where its four
siblings are specific, and it has exactly one construction site, reached by exactly one comparison.
`VersionTooOldAcquireError` names that comparison.

It also stops carrying a `Text`. The other four constructors carry a message because a message is
all the driver has; this one has the actual numbers, and rendering them into prose only to make the
caller parse them back is the same loss of structure the initialization split exists to avoid. The
three `Int`s are the server's major, minor and patch, in that order. The minimum is not carried
alongside them: it is a constant of this library - `ServerVersion.minimum`, currently 9.0.0 - so a
caller that needs it reads it from the library rather than from every error value, and a
minimum-and-actual pair would commit the API to a shape that says nothing an accessor does not.

That the payload duplicates the internal `Hasql.Connection.ServerVersion` is deliberate for now.
Publishing the constructor as three `Int`s keeps `ServerVersion` an implementation detail; promoting
it to the public API is a separate decision, and one worth making on its own merits rather than as a
side effect of naming an error.

## The residual is scoped, and there is no catch-all

`ConnectionAcquireError Text` means: the connection could not be established, and no structured
signal exists to say why. That is the old `OtherAcquireError`, and pretending otherwise would be
dishonest.

What changes is its reach. `OtherAcquireError` was residual across the whole of `acquire`: a
session-initialization failure landed in it, because no substring matched, and so did a rejected
password, and a caller receiving it learned nothing about how far acquisition had got.
`ConnectionAcquireError` is residual within one stage and cannot absorb anything from another - an
initialization failure has two constructors of its own and reaches neither of them by falling
through.

The five constructors therefore partition `acquire`'s failure modes exhaustively, and no catch-all
is retained as a safety valve. That is worth doing on purpose rather than by omission. A residual
constructor that nothing constructs is not inert: it is where the next stage added to `acquire` will
land if nobody thinks about it, and a reviewer has no way to notice, because the catch-all was
always going to be reached by something. Without one, adding a stage forces a constructor and a
decision about what that stage observes. "There is no uncategorized case" is a stronger statement
about a driver's error type than any documentation, and it is only true while nothing can quietly
make it false.

## Rejected alternatives

1. **Keep the old constructors, fix the classifier.** Match on more patterns, or normalize the
   locale by setting `LC_MESSAGES=C` on the connection before classifying. Rejected because the
   locale is only one of the three independent reasons the text is unsound, and forcing it is not
   the driver's call to make: `libpq` reads the process environment, so a driver that overrode it
   would be changing the language of messages the application also shows its own users.
2. **Derive an authentication constructor from `connectionNeedsPassword || connectionUsedPassword`.**
   Rejected on `usedPassword`, for the reason given above: it is true in cases that are not
   authentication failures, so the constructor would misreport precisely the failures that matter.
   The half of it that is sound is kept.
3. **The grouped shape of ADR 0004**, with `needsPassword` as a `Bool` field and the initialization
   pair as an `Either`. Rejected on the three counts in "Constructors, not fields".
4. **A single opaque constructor carrying the message.** The most minimal reading of "publish only
   what you observe". Rejected because stage and `needsPassword` *are* observed - the latter is a
   flag, not an inference - and the rule is about the provenance of a distinction, not about
   minimizing the count. It would throw away the one signal that survives the argument this document
   makes.
5. **A cause enum beside a common message field**, `data AcquireError = AcquireError Cause Text`.
   Invited by the observation that four of the five constructors carry the same `Text`. Rejected for
   the reasons that rule out 0004's shape - a second exported type and a two-level read, for a
   five-member sum - and because the payloads are not in fact common:
   `InitializationServerErrorAcquireError` carries a `ServerError`, not a message, so the field
   would have to be optional or the server case would have to sit outside the record.
6. **`UndiagnosedConnectionAcquireError` for the residual case.** Rejected under the naming rule
   above, which achieves the same guard more cheaply.
7. **Dropping the `Initialization` prefix from the two initialization constructors.** A connection
   *loss*, as distinct from a failure to establish one, can only be observed during initialization,
   so `ConnectionLossAcquireError` would be unambiguous on its own. Rejected because the prefix is
   where the stage information lives once the grouping is gone.
8. **Naming the stages after `libpq`'s protocol phases** - `HandshakeAcquireError`. Rejected because
   the first stage covers DNS resolution and TCP connection as well, which precede any handshake;
   `could not translate host name` is not a handshake failure. `Connection` names what the stage is
   for.
9. **Keeping a rendered `Text` on `VersionTooOldAcquireError`**, for uniformity with its four
   siblings. Rejected because uniformity is not the property that matters here: the siblings carry
   prose because prose is all that exists at those sites, and this one has numbers. A caller that
   wants to log a version string can format three `Int`s; a caller that wants to compare them cannot
   unformat a `Text`.

## What breaks

`AcquireError` has not shipped in a release - it exists only in the `Upcoming` section of the
changelog, introduced there alongside `UseError`. As with ADR 0003, this is a revision of unreleased
work, so the changelog entry is rewritten to describe the end state rather than layering a second
breaking change over the first.

| Site | Effect |
|---|---|
| `Hasql.Engine.Errors.AcquireError` | `NetworkingAcquireError`, `AuthenticationAcquireError` and `OtherAcquireError` removed; `CompatibilityAcquireError` renamed to `VersionTooOldAcquireError` and its `Text` replaced by the three `Int`s of the server version; `ConnectionAcquireError`, `ConnectionPasswordRequiredAcquireError`, `InitializationConnectionLossAcquireError` and `InitializationServerErrorAcquireError` added |
| `Hasql.Connection.acquire` | `interpretConnectionError` and both substring lists deleted; the version-check site passes the `ServerVersion` components through instead of formatting them with `ServerVersion.toText`; the status-check site reads `connectionNeedsPassword` to choose between `ConnectionPasswordRequiredAcquireError` and `ConnectionAcquireError`; the initialization site reads `resultErrorField` to build a `ServerError`, falling back to `InitializationConnectionLossAcquireError` when no SQLSTATE is present |
| `Hasql.Errors.IsError` | the `AcquireError` instance overrides `toSqlState`, delegating to `InitializationServerErrorAcquireError`'s payload |
| `Integration.Isolated.Connection.Acquire.ErrorClassificationSpec` | deleted whole: both its tests target the classifier, and its apparatus - a re-exec of the test binary under `LC_ALL=fr_FR.UTF-8`, and a bespoke `docker run` with `max_connections=1` - existed only to exercise locale sensitivity and the pattern list |
| `Integration.Isolated.Connection.AcquireSpec` | the `AuthenticationAcquireError` cases become the two live-server tests that pin `needsPassword`'s semantics: no password against a password-demanding server reports `ConnectionPasswordRequiredAcquireError`, a *wrong* password reports `ConnectionAcquireError`; the `NetworkingAcquireError` cases assert `ConnectionAcquireError` |
| `Pure.Connection.AcquireSpec` | one test per constructor, through the existing fake adapter; `connectionNeedsPassword` stops being `unimplementedIO` on `fakeConnection` |
| `Pure.ErrorsSpec` | rendering assertions for the removed constructors replaced |
| `pqi-native` | its `connectionNeedsPassword` returning a constant is a divergence from `libpq`, fixed upstream rather than accommodated here |
