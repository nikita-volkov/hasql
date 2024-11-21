# 1.9.0 (unreleased)

- Add `Hasql.Connection.withConnection`

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
