# `AcquireError` is a flat set of observed failures

**Status: an alternative to [ADR 0004](0004-acquireerror-is-organized-by-stage-not-by-inferred-cause.md),
not yet chosen. Both documents are live; exactly one will remain so.**

`AcquireError` becomes five constructors on one level, each named for what happened:

```haskell
data AcquireError
  = ConnectionAcquireError Text
  | PasswordRequiredAcquireError Text
  | VersionTooOldAcquireError Text
  | InitializationConnectionLossAcquireError Text
  | InitializationServerErrorAcquireError ServerError
```

This document accepts everything ADR 0004 argues about *which facts this type may publish* and
rejects only the shape 0004 derived from it. The two documents agree on the deletions - the
networking and authentication constructors, both substring lists, `interpretConnectionError` - and on
the fact set that replaces them. They disagree about whether those facts should be constructors or
fields.

## What carries over from 0004 unchanged

0004's argument runs: a failed `PQconnectdb` yields no `PGresult`, therefore no `PG_DIAG_*` fields
and no SQLSTATE, therefore the only signal available is `libpq`'s message text - which is
`gettext`-translated against the client locale, is explicitly excluded from the `Pqi` adapter
contract, and is consequently not a thing a driver may branch on. Against that, four distinctions
survive as publishable, because the driver observes rather than infers them: which stage of
`acquire` failed, `PQconnectionNeedsPassword`, the server-version comparison Hasql performs itself,
and whether an error report carries a SQLSTATE. `PQconnectionUsedPassword` is rejected, since it
stays true when authentication succeeded and the connection failed later for an unrelated reason.

All of that stands. It is the epistemology of the type - what may be said - and this document does
not revisit it. Read 0004 for the full argument; it is not repeated here.

## Where 0004 overreaches

0004 proves a claim about facts and then applies it as a claim about shape. Having established that
*stage* is publishable and *inferred cause* is not, it concludes that constructors should therefore
*be* stages, with every other fact demoted to a field:

```haskell
  = ConnectionAcquireError Bool Text
  | CompatibilityAcquireError Text
  | InitializationAcquireError (Either Text ServerError)
```

That step does not follow. The flat set above publishes precisely the same facts - nothing is lost,
nothing is added. `PasswordRequiredAcquireError` is 0004's `Bool`; the initialization pair is 0004's
`Either`. The two shapes are isomorphic. So the choice between them cannot be settled by the
observability argument, which both satisfy equally, and has to be settled on what the shape costs a
reader.

On that question the flat set wins on three counts and, as far as this document can determine, loses
on none.

**Named cases instead of a positional `Bool` and an unnamed `Either`.** `ConnectionAcquireError Bool
Text` requires haddock to explain what `True` means, at the definition and again wherever anyone
matches it. `Either Text ServerError` requires haddock to say which side is the connection-died
case. `PasswordRequiredAcquireError` and `InitializationServerErrorAcquireError` require neither.
A type whose shape has to be annotated to be read is a type that could have been shaped better.

**One match level.** Rendering an `AcquireError` under 0004 means three top-level alternatives, then
a `Bool` inspection inside one and an `Either` match inside another. Under the flat set it is five
alternatives and no inner matching. The exhaustiveness checker covers all five directly, rather than
covering three and leaving the inner two shapes to be handled by hand.

**The nesting has nothing to nest.** Hierarchy earns its keep when many cases share a level and the
grouping saves a reader from reading them all. Here the connection stage holds two cases and the
initialization stage holds two. A two-level structure over four leaves is the weakest instance of
the pattern there is, and the stage information is not lost by flattening - it moves into the
constructor names, where `InitializationServerErrorAcquireError` still tells a caller that DNS
resolved, credentials were accepted, and the server version passed.

## The one defense of nesting, and why it is disqualified

The strongest argument for the grouped shape is that it lets a caller handle a whole stage at once:
match `ConnectionAcquireError` and treat every way of failing to connect uniformly, without
enumerating them.

That convenience is real, and it is a retry-grouping convenience - the caller who wants it wants it
because connection-stage failures are the ones worth another attempt against a pool.
[ADR 0003](0003-the-fatal-side-of-useerror-needs-no-subdivision.md) settles that this is not a
consideration Hasql designs around: retryability depends on the caller's policy and topology, which
the driver cannot see, and a driver that shapes its error type to make one particular retry
partition convenient has issued the verdict 0003 declines to issue, in a form that is harder to
notice than a boolean was. A caller who wants that partition can write it; enumerating two
constructors is not a hardship, and writing it themselves keeps the policy where it belongs.

Nor does nesting buy anything on evolution. The apparent advantage - a new `libpq` flag enriches the
`Bool` into a sum without disturbing the top level - is not an advantage, because widening a
positional `Bool` into a sum type is exactly as breaking for callers as adding a constructor. Both
shapes cost the same to iterate, which is what the type should expect to do as the observable facts
change.

## The residual is scoped rather than removed

`ConnectionAcquireError Text` means: the connection could not be established, and no structured
signal exists to say why. That is the old `OtherAcquireError`, and pretending otherwise would be
dishonest.

What changes is its reach. `OtherAcquireError` was residual across the whole of `acquire`: a
session-initialization failure landed in it, because no substring matched, and so did a rejected
password, and a caller receiving it learned nothing about how far acquisition had got.
`ConnectionAcquireError` is residual within one stage and cannot absorb anything from another - an
initialization failure has two constructors of its own and reaches neither of them by falling
through. The set remains a total decomposition of `acquire`'s failure modes, so a stage added later
still forces a constructor and a decision about what that stage observes, which is 0004's
no-catch-all property preserved intact.

## Rejected alternatives

1. **The grouped shape of ADR 0004.** Rejected on the three counts above. Its argument about which
   facts are publishable is adopted wholesale; only the taxonomy is rejected.
2. **`UndiagnosedConnectionAcquireError` for the residual case.** Names a fact about available
   evidence rather than about the failure, and echoes `libpq`'s own `PG_DIAG_*` vocabulary - the
   constructor exists precisely because no diagnostic fields are produced for a failed
   `PQconnectdb`. Rejected as a poor trade: it is long and awkward, and the risk it guards against -
   a future maintainer of `acquire` reaching for the plain-sounding constructor when the password
   case applies - falls on one construction site inside this library, not on callers. This does
   leave the type with four cause-named constructors and one named for its stage, which is the one
   place the naming axis breaks. That is an accepted cost, recorded here rather than argued away.
3. **`CompatibilityAcquireError`, kept from the current API.** Rejected under the flat set's naming
   axis: it is vague where its four siblings are specific, and it has exactly one construction site,
   reached by exactly one comparison. `VersionTooOldAcquireError` names that comparison. The
   constructor still carries only a `Text` - a structured minimum-and-actual pair was considered and
   rejected as committing the API to more than the situation needs.
4. **Dropping the `Initialization` prefix from the two initialization constructors.** A connection
   *loss*, as distinct from a failure to establish one, can only be observed during initialization,
   so `ConnectionLossAcquireError` would be unambiguous on its own. Rejected because the prefix is
   where the stage information lives once the grouping is gone, and it is the most valuable thing
   those two constructors say: everything before initialization succeeded.
5. **A single opaque constructor carrying the message.** The most minimal reading of "publish only
   what you observe". Rejected for the same reason 0004 rejects it: stage and `needsPassword` are
   observed, not inferred, and the rule governs the provenance of a distinction rather than the
   number of them.

## What breaks

`AcquireError` has not shipped in a release - it exists only in the `Upcoming` section of the
changelog, introduced there alongside `UseError`. As with ADR 0003, this is a revision of unreleased
work, so the changelog entry is rewritten to describe the end state rather than layering a second
breaking change over the first.

| Site | Effect |
|---|---|
| `Hasql.Engine.Errors.AcquireError` | `NetworkingAcquireError`, `AuthenticationAcquireError` and `OtherAcquireError` removed; `CompatibilityAcquireError` renamed to `VersionTooOldAcquireError`; `ConnectionAcquireError`, `PasswordRequiredAcquireError`, `InitializationConnectionLossAcquireError` and `InitializationServerErrorAcquireError` added |
| `Hasql.Connection.acquire` | `interpretConnectionError` and both substring lists deleted; the status-check site reads `connectionNeedsPassword` to choose between `PasswordRequiredAcquireError` and `ConnectionAcquireError`; the initialization site reads `resultErrorField` to build a `ServerError`, falling back to `InitializationConnectionLossAcquireError` when no SQLSTATE is present |
| `Hasql.Errors.IsError` | the `AcquireError` instance overrides `toSqlState`, delegating to `InitializationServerErrorAcquireError`'s payload |
| `Integration.Isolated.Connection.Acquire.ErrorClassificationSpec` | deleted whole: both its tests target the classifier, and its apparatus - a re-exec of the test binary under `LC_ALL=fr_FR.UTF-8`, and a bespoke `docker run` with `max_connections=1` - existed only to exercise locale sensitivity and the pattern list |
| `Integration.Isolated.Connection.AcquireSpec` | the `AuthenticationAcquireError` cases become the two live-server tests that pin `needsPassword`'s semantics: no password against a password-demanding server reports `PasswordRequiredAcquireError`, a *wrong* password reports `ConnectionAcquireError`; the `NetworkingAcquireError` cases assert `ConnectionAcquireError` |
| `Pure.Connection.AcquireSpec` | one test per constructor, through the existing fake adapter; `connectionNeedsPassword` stops being `unimplementedIO` on `fakeConnection` |
| `Pure.ErrorsSpec` | rendering assertions for the removed constructors replaced |
| `pqi-native` | its `connectionNeedsPassword` returning a constant is a divergence from `libpq`, fixed upstream rather than accommodated here |
