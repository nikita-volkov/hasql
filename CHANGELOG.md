# 1.10

## Breaking changes

- Decoder checks are now more strict and report `DecoderTypeMismatch` when the actual type of a column does not match the expected type of a decoder. Previously such mismatches were silently ignored and could lead to either autocasts or runtime errors later on.
  - E.g., `int4` column decoded with `int8` decoder will now report `DecoderTypeMismatch` instead of silently accepting the value.

- Session now has exclusive access to the connection for its entire duration. Previously it was releasing and reacquiring the lock on the connection between statements.

- Dropped `MonadReader Connection` instance for `Session`.

- Dropped `Monad` and `MonadFail` instances for `Row`. Applicative is enough for all practical purposes.

- `UnexpectedAmountOfColumns` error added to `RowError` type. It is reported when there are not enough columns in the result for the decoders used.

- `DecoderTypeMismatch` error added to `ResultError` type. It is reported when the actual type of a column does not match the expected type of a decoder.

- `EndOfInput` error removed. Now if there are not enough columns in the result, `UnexpectedAmountOfColumns` error is reported instead.

- `ServerError` got a new field: Causal execution status.

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
