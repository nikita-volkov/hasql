# Upcoming

## Fixes

- `Hasql.Connection.acquire`'s `ConnectionError` classification recognized far fewer transient failures than it should have: `"the database system is starting up"`, `"the database system is in recovery mode"`, `"sorry, too many clients already"`, `"server closed the connection unexpectedly"`, `"connection reset by peer"`, `"network is unreachable"`, `"no route to host"`, `"connection timed out"`, `"could not fork new process"`, and `"terminating connection due to administrator command"` all previously fell through to `OtherConnectionError`, so `isTransient` reported `False` for what are, in every case, worth retrying. Conversely, `"no such file or directory"` (a missing Unix-socket path — a misconfiguration, not a networking hiccup) was wrongly grouped with the transient cases; it's now `OtherConnectionError`. (#329)

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
