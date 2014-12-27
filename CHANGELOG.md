# 0.6.0 - Major API overhaul
* Removed the `Session` monad, opening a direct access to the connection pool, thus providing for a simpler compatibility with other libraries.
* The connection timeout is now set using `Int` for simplicity.
* There are no exceptions any more. All the error-reporting is typed and done explicitly, using `Either`.
* The error types are now mostly backend-specific.
* The transaction mode is now extended to support uncommitable transactions with the `TxWriteMode` type.
* `Tx` now has a `MonadError` instance, which allows to handle errors while remaining in the transaction.
* All `Tx` functions now have a "Tx" suffix.
* There is no more `list` transaction. Instead there is `vectorTx`. 
* The `Statement` type is renamed to `Stmt` and is now exported from the main API.
* `RowParser` is now uninstantiable. This enforces the idiomatic usage of the library.

# 0.5.0
* Update the "list-t" and "monad-control" deps

# 0.4.1
* Fix the transaction conflicts bug
