# 1.7

- Error model completely revised:
  - All sum-types now follow the convention of having constructors suffixed with the type name. In particular the following transformations occurred:
    - `QueryError` type got renamed to `SessionError`
    - `QueryError` constructor got renamed to `QuerySessionError`
    - New `PipelineSessionError` constructor got added to the `SessionError` type
    - `ClientError` constructor got renamed to `ClientCommandError`
    - `ResultError` constructor got renamed to `ResultCommandError`
    - `ServerError` constructor got renamed to `ServerResultError`
    - `UnexpectedResult` constructor got renamed to `UnexpectedResultError`
    - `RowResult` constructor got renamed to `RowResultError`
    - `RowError` type got renamed to `ColumnError`
    - `EndOfInput` constructor got renamed to `EndOfInputColumnError`
    - `UnexpectedNull` constructor got renamed to `UnexpectedNullColumnError`
    - `ValueError` constructor got renamed to `UnexpectedNull`
    - New `RowError` type got created
    - `RowResult` constructor's column field go transferred into the new `RowError` type

- Decidable instance on `Encoders.Params` removed. It was useless and limited the design.

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
