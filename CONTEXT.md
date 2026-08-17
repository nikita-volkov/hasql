# Context

Glossary of the domain language used in hasql. Terms only — no implementation
details, no decisions (those live in `docs/adr/`).

## Statement

A specification of a single, parameterizable SQL query: an SQL template plus the
encoder for its parameters and the decoder for its result. Defined once and
executed many times with differing parameter values. Whether a given execution
ends up **prepared** is a connection-level policy decision (see **statement
cache**), not a property the Statement itself carries.

## Params (parameters encoder)

The encoder half of a Statement: maps a Haskell value to the ordered sequence of
SQL parameters (`$1`, `$2`, …). Composes monoidally — two encoders concatenate
their parameters.

## Parameter metadata

The static, value-independent description of each parameter a Params encoder
produces: its **type reference**, array dimensionality, and wire format
(binary/text). Knowable from the encoder alone, before any parameter value or
OID resolution.

> Naming note: the code currently calls this `columnsMetadata`, but it describes
> *parameters*, not result columns.

## Qualified type name

A Postgres type identified by name: an optional schema together with a required
type name. `Nothing` schema means unqualified (resolved via the search path).
Used pervasively as the key under which a type's OIDs are looked up.

> Currently modelled as the bare tuple `(Maybe Text, Text)`.

## Type reference

How a parameter's Postgres type is identified within parameter metadata: either
an already-known **OID**, or a **qualified type name** still pending OID
resolution.

> Currently modelled as `Either (Maybe Text, Text) Word32`.

## Type info / OID pair

A Postgres type's pair of OIDs: the base (scalar) OID and the OID of its array
type. Resolving a qualified type name yields one of these.

> Modelled by `TypeInfo {toBaseOid, toArrayOid}` — but the OID cache stores the
> bare tuple `(Word32, Word32)` instead, applying the concept inconsistently.

## Unknown types

The set of qualified type names referenced by a Statement (across both its
encoder and decoder) whose OIDs are not statically known and must be resolved
against the server before execution.

## OID cache

A connection-scoped mapping from qualified type name to its resolved OID pair,
populated by resolving unknown types and reused across statements on that
connection.

## Statement cache

A connection-scoped, bounded LRU cache mapping a **local key** (SQL text plus
parameter OIDs) to the server-side prepared plan behind it. Governed by two
connection settings: `statementCacheSize` (capacity; `0` disables preparation
entirely) and `prepareThreshold` (executions of the same local key required
before it is admitted, i.e. actually `PARSE`d and kept server-side). Below the
threshold, or once eviction has dropped an entry, an execution falls back to an
unprepared roundtrip — this is invisible to the caller and does not affect
correctness, only round-trip count.

## Local key

The client-side identity of a statement in the statement cache: its SQL
template together with the OIDs its parameters resolved to. The OIDs are part of
the identity because the same SQL prepared under different parameter types is a
different statement on the server.

## Remote key

The name a statement is prepared under on the server — what a `DEALLOCATE`
names, and what distinguishes two server-side plans for the same local key
prepared at different times. Allocated by the connection and never reused while
the server may still hold the statement.

## Admission

A statement crossing the prepare threshold and thereby entering the statement
cache: `PARSE`d under a fresh remote key and kept server-side. The counterpart
of eviction, and the reason the cache is not a plain LRU — without an admission
policy a one-shot dynamic statement would occupy a slot and displace a hot one
on its very first execution.

## Eviction

A statement leaving the statement cache — either to make room for an admission
once the cache is at capacity, or because the server invalidated it. Names the
client-side drop; whether the server-side statement is also `DEALLOCATE`d
depends on whether the server is known to still hold it.

## Desync

The condition of the statement cache possibly disagreeing with the server about
what is prepared, which no snapshot can resolve — as after a send-side failure,
where it is unknown whether the batch reached the server at all. Resolved only
by clearing both sides with a `DEALLOCATE ALL`.

## Tag

A value attached to a protocol action when it is constructed, identifying the
statement it performs: its SQL, rendered parameters, position in a batch, and
whether it is prepared. If the action fails, its tag travels with the error,
so the error can name the statement that caused it (and, in pipelines, recover
the statement-cache snapshot taken when the action was issued). Actions
performed by the driver itself carry no tag, marking their errors as belonging
to no user statement. Unrelated to the execution contexts — **Session** and
**Pipeline**.

