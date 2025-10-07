# 1.10

Major revision happened.

## New Features

- **Decoder compatibility checks**.

  Previously decoders were silently accepting values of different types, if binary decoding did not fail. Now decoders check if the actual type of the column matches the expected type of the decoder and report `DecoderTypeMismatch` error if they do not match. They also match the amount of columns in the result with the amount of columns expected by the decoder and report an error if they do not match.

- **Flat error model**.

  Previously errors were nested according to the library's internal context they happened in. The user had to unwrap multiple layers of errors to get to the actual reason. Now the reason is at the top level and the context is described by the `Location` types.

## Breaking changes

- Decoder checks are now more strict and report `DecoderTypeMismatch` when the actual type of a column does not match the expected type of a decoder. Previously such mismatches were silently ignored and could lead to either autocasts or runtime errors later on.
  - E.g., `int4` column decoded with `int8` decoder will now report `DecoderTypeMismatch` instead of silently accepting the value.

- Due to the above the oldest supported PostgreSQL version now is 10. In older versions some types had different OIDs.

- Session now has exclusive access to the connection for its entire duration. Previously it was releasing and reacquiring the lock on the connection between statements.
  - If you need the old behaviour, you can either use "hasql-pool" or use `ReaderT Connection (ExceptT SessionError IO)`.

- Dropped `MonadReader Connection` instance for `Session`.

- Dropped `Monad` and `MonadFail` instances for `Row`. `Applicative` is enough for all practical purposes.

- Errors model completely redesigned to be flat with context being described by the location types.

- `ConnectionError` restructured and moved from the `Hasql.Connection` module to `Hasql.Errors`.

- `SessionError` restructured and moved from the `Hasql.Session` module to `Hasql.Errors`.

- `usePreparedStatements` setting dropped. Use `disablePreparedStatements` instead.

- `Hasql.Session.sql` is renamed to `Hasql.Session.script` to better reflect its purpose.

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
