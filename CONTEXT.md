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

A connection-scoped mapping from a **local key** (SQL text plus parameter OIDs)
to the **remote key** the statement is prepared under on the server. A
preparable statement is `PARSE`d on its first execution and served from the
cache on every execution after that.

The cache is unbounded: no capacity, no admission threshold, no eviction policy.
It grows with the number of distinct statements the connection has executed.
Whether a statement is prepared at all is a property of the statement and of the
connection's `noPreparedStatements` setting, not of the cache.

## Local key

The client-side identity of a statement in the statement cache: its SQL
template together with the OIDs its parameters resolved to. The OIDs are part of
the identity because the same SQL prepared under different parameter types is a
different statement on the server.

## Remote key

The name a statement is prepared under on the server — what a `DEALLOCATE`
names, and what `pg_prepared_statements` lists.

It is content-addressed: `hasql_` followed by the first 57 hex characters of the
SHA-256 of the statement's serialization (the SQL length, the SQL, the parameter
count, then each parameter OID), for a 63-byte name that exactly fills the
identifier limit. So it is a pure function of the local key, and deliberately
stable — the same statement gets the same name on every connection, in every
process, and a statement re-prepared after its entry was dropped comes back
under the name it had before. That stability is what lets a pooler recognise a
statement it has already prepared on a given server connection.

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

