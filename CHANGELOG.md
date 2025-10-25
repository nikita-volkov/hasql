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

## Breaking changes

- Custom type mappings (enums and composite types) now require specifying names for the types being mapped.
  - This will automatically identify the types with the DB and do deep compatibility checks.

- Decoder checks are now more strict and report `UnexpectedColumnTypeStatementError` when the actual type of a column does not match the expected type of the decoder. Previously such mismatches were silently ignored and could lead to either autocasts or runtime errors in later stages.
  - E.g., `int4` column decoded with `int8` decoder will now report `UnexpectedColumnTypeStatementError` instead of silently accepting the value.

- Due to the above the oldest supported PostgreSQL version now is 10. In older versions some types had different OIDs.

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
