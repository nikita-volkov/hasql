# v2.1.0.0-rc

## Breaking

- `Hasql.Decoders.rowVector` now decodes into any `Data.Vector.Generic` vector (`Vector`, unboxed, storable, primitive) instead of only the boxed `Data.Vector.Vector`, picked by the type the result is consumed as. Call sites that left the vector type to be inferred with nothing pinning it down (no type signature, no boxed-`Vector`-specific downstream use) will now hit an ambiguous type error and need an explicit annotation. (#336)

- `Hasql.Connection.use` now returns `UseError` instead of a single session-error type. `UseError` has two constructors:

  - `SessionUseError` wraps a `SessionError` - the session ran but reported a recoverable error (a server error, a decode failure). The connection is still live.
  - `ConnectionUseError` - the connection is gone, for any reason: a dropped socket, a request libpq refused to send outright, an unexpected response, a bug in Hasql. `use` has closed the handle before returning; every subsequent `use` on the same `Connection` reports this error again.

  Code that pattern-matched on the old session-error type now matches on `UseError`. The `SessionUseError` arm covers the recoverable errors that `SessionError` already expressed; `ConnectionUseError` replaces every constructor that meant the connection was gone.

- `Hasql.Errors.IsError` no longer has an `isTransient` method. Whether a failure is worth retrying depends on the caller's retry policy and, for server errors, the SQLSTATE reported through `toSqlState` - not on a verdict the driver hands out. No replacement is provided.

- `Hasql.Connection.acquire` now returns `AcquireError` instead of a plain `Text` message. The five constructors are organized by which stage of `acquire` failed - connecting, checking the server version, or initializing session settings - since that is what the driver actually observes, rather than by a best-effort classification of `libpq`'s (locale-dependent, adapter-dependent) error text:

  - `ConnectionAcquireError` - the connection could not be established, and no structured signal exists to say why. This is the residual case at the connection stage: DNS failure, connection refused, TLS negotiation failure, a rejected password, a missing database, and any other rejection all land here alike.
  - `ConnectionPasswordRequiredAcquireError` - the server demanded a password and none was available, reported from `libpq`'s own flag rather than inferred from message text.
  - `VersionTooOldAcquireError` - the server's version is below the minimum this driver supports, carrying the server's major, minor and patch version as `Int`s rather than formatted prose.
  - `InitializationConnectionLossAcquireError` - session initialization failed and the connection died, with prose and nothing else.
  - `InitializationServerErrorAcquireError` - session initialization failed and the server said why, carrying a structured `ServerError` with a SQLSTATE.

- A `ConnectionUseError` now closes the connection before returning. `Hasql.Connection.release` on a spent handle does nothing. Pools already discarded connections on fatal errors, so this makes the driver keep the contract its callers were already assuming.

  As a consequence `Hasql.Connection.release` is idempotent, and a released connection can no longer be reached through `use`. Both were undefined behaviour before: `libpq` forbids touching a connection after `PQfinish`, and the driver already closed connections it could not clean up.

  The driver no longer attempts to repair a connection it failed to send on. There is nothing it can assume about the protocol state of such a connection, and the only repair available - pushing a Sync - has the side effect of flushing and committing whatever the failed session had queued. Closing it is both simpler and more honest.

- An exception that cuts a session short - one thrown by the session itself, or an interruption delivered from another thread by `timeout`, `race` or `killThread` - now closes the connection. It propagates as before, but the handle it fired on is spent from then on, so a `timeout` around a session costs a connection rather than returning one. Pools reconnect; code holding a `Hasql.Connection.Connection` directly has to acquire a new one. Server-side session state - anything `set` on the connection - goes with it, where it used to be deliberately preserved across an interruption.

  The driver used to bring such a connection back to a clean state instead - draining results, aborting the transaction, deallocating prepared statements. That repair is blocking network IO performed under a mask, so on a connection whose peer has gone away it never returns and the very interruption being handled never lands. It could also report its own failure as a returned error without rethrowing, which made `timeout` yield `Just (Left _)` instead of `Nothing`. Neither is worth the connection it saves.

## Non-breaking

- Prepared statement names are now content-addressed (`hasql_` plus a SHA-256 digest of the SQL and parameter OIDs) rather than per-connection counters. The Haskell API is unchanged. (#324)

- `42P05` ("prepared statement already exists") no longer forces the driver to evict the statement cache entry, and the mapping survives it. (#331)

## Fixes

- `Hasql.Connection.acquire` now finishes the underlying connection on every failure path, and no longer ignores the result of its session-init statements.

- Send requests libpq refuses outright (e.g. over 65535 parameters) now close the connection, so a retry wrapper cannot resend the identical rejected request against the same handle forever. (#327)

- A send failure partway through a pipeline left the connection stuck in pipeline mode with undrained results, so every later session on it failed too. The connection is now closed rather than handed back. (#326)

- A pipeline whose send fails partway through no longer applies the statements preceding the failure. In pipeline mode libpq buffers until a Sync, so those statements had not reached the server; what put them there was the recovery attempt, whose Sync flushed and committed them on the way to reclaiming the connection. Such a pipeline is now discarded whole, matching what a pipeline that fails on a server error already did.

- A session that catches a pipeline failure and carries on no longer runs the rest of itself against a connection the send failed on. The remaining operations report `ConnectionUseError` instead, and the connection is closed however the session ends - including when it swallows the failure and succeeds. Previously the first serial statement after such a catch blocked forever on results the server had never been asked for.

- A connection lost while receiving results is now reported as `ConnectionUseError`, matching how the send side already classified a lost socket. It used to surface as a `StatementSessionError` carrying `UnexpectedRowCountStatementError` - "expected 1 row, got 0" - which is not transient, so retry wrappers did not retry it and pools returned the dead connection for reuse.

# v2.0.1.0

## New Features

- `IsError` gained a `toSqlState` method, exposing the SQLSTATE the server reported for an error, or `Nothing` where the error carries no server code. It saves consumers from pattern-matching their way down to the nested `ServerError` — a dig that has to be rewritten every time the error types gain a constructor.

  ```haskell
  case Errors.toSqlState err of
    Just "23505" -> handleUniqueViolation
    _ -> rethrow err
  ```

  The method has a default implementation returning `Nothing`, so existing instances keep compiling. Instances for error types that *wrap* another error type must override it and delegate to the wrapped value, otherwise they silently report `Nothing` for codes they do carry.

# v2.0.0.3

## Fixes

- Work around the bug in Cabal due to which documentation does not get generated for definitions reexported from sublibs.

# v2.0.0.2

Work around the bugs in Cabal/Haddock that cause missing documentation for two-hop reexported internal modules.

# v2.0.0.1

Support for `pqi-1.1`.

# v2.0.0.0

New era: the transport layer is now pluggable via [`pqi`](https://github.com/nikita-volkov/pqi), and an alpha pure-Haskell backend, [`pqi-native`](https://github.com/nikita-volkov/pqi-native), is available for early adopters. Goal: a reliable, performant, no-C-dependency replacement for libpq.

## Breaking

- `Hasql.Connection.acquire` now takes an explicit adapter as its first argument, ahead of `Settings`. To keep prior behaviour, depend on [`pqi-ffi`](https://hackage.haskell.org/package/pqi-ffi) and pass `Pqi.Ffi.adapter`. To try the native backend, depend on [`pqi-native`](https://hackage.haskell.org/package/pqi-native) and pass `Pqi.Native.adapter`.

# 1.10

Major revision happened.

## New Features

- **OID by name resolution**.

  Encoders and decoders now support resolving PostgreSQL type OIDs by their names at runtime. This enables working with custom types (enums, composite types, domains) without hardcoding OID values. The system includes an OID cache to optimize repeated lookups and automatically queries `pg_type` and related system catalogs when needed. This change affects array, composite, and value encoders/decoders throughout the codec system.

- **Decoder compatibility checks**.

  Previously decoders were silently accepting values of different types, if binary decoding did not fail. Now decoders check if the actual type of the column matches the expected type of the decoder and report `UnexpectedColumnTypeStatementError` error if they do not match. They also match the amount of columns in the result with the amount of columns expected by the decoder and report an error if they do not match.

- **No resets on errors**.

  Previously when an async exception was raised during the execution of a session, the connection would get reestablished to recover from any possible half-finished states. That led to a loss of the connection-local state on the server side. Now the connection recovers without resetting.

- **Redesigned connection configuration API**.

  The connection settings API has been completely redesigned to be more composable and user-friendly. Settings are now represented as a monoid, allowing easy combination of multiple configuration options. The API now supports both URI and key-value connection string formats, with individual setters for common parameters like host, port, user, password, etc.

- **Custom codec API**.

  Added `Hasql.Encoders.custom` and `Hasql.Decoders.custom` functions providing a low-level API for defining custom value encoders and decoders. These functions offer fine-grained control over OID resolution, allowing you to:
  - Specify static OIDs when known at compile time
  - Automatically resolve OIDs at runtime by type name
  - Declare dependencies on other types needed for serialization/deserialization (e.g., field types in composite types)
  - Implement custom binary encoding/decoding logic with access to resolved OIDs

  This is particularly useful for advanced use cases like custom composite types with field validation or specialized binary formats.

## Breaking changes

- Text instead of ByteString for textual data.
  - The public API now uses `Text` instead of `ByteString` for SQL statements and error messages.

- Custom type mappings (enums and composite types) now require specifying names for the types being mapped.
  - This will automatically identify the types with the DB and do deep compatibility checks.

- Decoder checks are now more strict and report `UnexpectedColumnTypeStatementError` when the actual type of a column does not match the expected type of the decoder. Previously such mismatches were silently ignored and could lead to either autocasts or runtime errors in later stages.
  - E.g., `int4` column decoded with `int8` decoder will now report `UnexpectedColumnTypeStatementError` instead of silently accepting the value.

- Session now has exclusive access to the connection for its entire duration. Previously it was releasing and reacquiring the lock on the connection between statements.
  - If you need the old behaviour, you can use `ReaderT Connection (ExceptT SessionError IO)`.

- Dropped `MonadReader Connection` instance for `Session`.

- Dropped `Monad` and `MonadFail` instances for the `Row` decoder. `Applicative` is enough for all practical purposes.

- Errors model completely overhauled.
  - `ConnectionError` restructured and moved from the `Hasql.Connection` module to `Hasql.Errors`.
  - `SessionError` restructured and moved from the `Hasql.Session` module to `Hasql.Errors`.

- `usePreparedStatements` setting dropped. Use `disablePreparedStatements` instead.

- `Hasql.Session.sql` renamed to `Hasql.Session.script` to better reflect its purpose.

- Connection configuration API overhaul to improve UX.
  - `Hasql.Connection.acquire` now takes a single `Settings` value instead of a list of `Setting` values.
  - The `Hasql.Connection.Setting` module has been replaced with `Hasql.Connection.Settings`.
  - Settings are now constructed using flat monoid composition instead of hierarchical lists requiring multiple imports.
  - Removed `Hasql.Connection.Setting.Connection` and related submodules.

- Custom value decoder signature changed.

  The `Hasql.Decoders.custom` function signature has been extended to support more explicit control over type resolution. It now requires:
  - Optional static OIDs parameter (previously implicit)
  - List of additional type dependencies needed for decoding
  - The decoder function now receives an OID lookup function as its first parameter

  This change enables more robust custom type handling but requires updating existing custom decoder implementations.

- Exception instances on error types removed. The error types here were never thrown as exceptions. Wrap them in your own exception type if you need to throw them.

# 1.9

- Revised the settings construction exposing a tree of modules
- Added a global prepared statements setting

## Why the changes?

To introduce the new global prepared statements setting and to make the settings API ready for extension without backward compatibility breakage.

## Instructions on upgrading the 1.8 code

### When explicit connection string is used

Replace

```haskell
Hasql.Connection.acquire connectionString
```

with

```haskell
Hasql.Connection.acquire 
  [ Hasql.Connection.Setting.connection (Hasql.Connection.Setting.Connection.string connectionString)
  ]
```

### When parameteric connection string is used

Replace

```haskell
Hasql.Connection.acquire (Hasql.Connection.settings host port user password dbname)
```

with

```haskell
Hasql.Connection.acquire
  [ Hasql.Connection.Setting.connection
    ( Hasql.Connection.Setting.Connection.params
      [ Hasql.Connection.Setting.Connection.Param.host host,
        Hasql.Connection.Setting.Connection.Param.port port,
        Hasql.Connection.Setting.Connection.Param.user user,
        Hasql.Connection.Setting.Connection.Param.password password,
        Hasql.Connection.Setting.Connection.Param.dbname dbname
      ]
    )
  ]
```

# 1.8.1

- In case of exceptions thrown by user from inside of Session, the connection status gets checked to be out of transaction and unless it is the connection gets reset.

# 1.8

- Move to "iproute" from "network-ip" for the "inet" datatype (#163).

# 1.7

- Decidable instance on `Encoders.Params` removed. It was useless and limited the design.
- `QueryError` type renamed to `SessionError`.
- `PipelineError` constructor added to the `SessionError` type.

# 1.6.3.1

- Moved to "postgresql-libpq-0.10"

# 1.6.3

- Added `unknownEnum` encoder

# 1.6.2

- Added composite encoder
- Added `oid` and `name` encoders

# 1.6.1

- Added `jsonLazyBytes` and `jsonbLazyBytes`

# 1.6

- Added position to `ServerError` (breaking change).
- Disabled failure on empty query.

# 1.5

- Added column number to `RowError` (breaking change).
- Added `MonadReader Connection` instance for Session.
