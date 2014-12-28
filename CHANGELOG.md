# 0.6.0 - Major API overhaul
* The connection pool acquisition is now explicit and is no longer handled by the `Session` monad. This should provide for a simpler integration with other libraries.
* The `Session` monad is now merely a convenience thing for providing a context to multiple transactions. One can run it as many times as he wants - it won't reestablish any resources any more.
* The connection timeout is now set using `Int` for simplicity.
* There are no exceptions any more. All the error-reporting is typed and done explicitly, using `Either`.
* The error types are now mostly backend-specific.
* The transaction mode is now extended to support uncommittable transactions with the `TxWriteMode` type.
* All `Tx` functions are now appended with a "Tx" suffix.
* Added `vectorTx` and `maybeTx` and updated the semantics of `singleTx`.
* `q` statement quasi-quoter is now renamed to more meaningful `stmt`.
* The `Statement` type is renamed to `Stmt` and is now exported from the main API.
* `RowParser` is now uninstantiable. This enforces the idiomatic usage of the library.
* Statement templates now support UTF-8.

# 0.5.0
* Update the "list-t" and "monad-control" deps

# 0.4.1
* Fix the transaction conflicts bug
