module Hasql.Private.TestUtils
where

import Hasql.Private.Prelude
import Hasql.Private.Connection
import Hasql.Private.Errors
import qualified Hasql.Private.Encoders as Encoders
import qualified Hasql.Private.Encoders.Params as Encoders.Params
import qualified Hasql.Private.IO as IO
import qualified Hasql.Statement as Statement
import qualified Hasql.Private.Connection as Connection

{-|
Prepare a 'Statement.Statement' to sanity check it against a schema.

To avoid requiring input parameter values, encoders are not used.
Instead, the types are determined by the query as described in the docs for
<https://hackage.haskell.org/package/postgresql-libpq-0.9.4.2/docs/Database-PostgreSQL-LibPQ.html#v:execParams postgresql-libpq>.

If using hasql-th, type casts in the query are already required, so the statements can be used as-is.

Note that this aims to catch many errors, but not all (e.g. field nullability is not checked).

E.g.,

Take the following statement (using hasql-th), which we assume matches our schema:

@
selectUserDetails :: Statement Int32 (Maybe (Text, Text, Maybe Text))
selectUserDetails =
  [maybeStatement|
    select name :: text, email :: text, phone :: text?
    from user
    where id = $1 :: int4
    |]
@

>>> tryPrepare conn selectUserDetails
Nothing

Now, we change it by using the wrong type for the id field:

@
selectUserDetails' :: Statement Text (Maybe (Text, Text, Maybe Text))
selectUserDetails' =
  [maybeStatement|
    select name :: text, email :: text, phone :: text?
    from user
    where id = $1 :: text
    |]
@
>>> tryPrepare conn selectUserDetails
Just (ResultError (ServerError "42883" "operator does not exist: integer = text" Nothing (Just "No operator matches the given name and argument types. You might need to add explicit type casts.")))

Or, we could change it to use a wrong column name:

@
selectUserDetails'' :: Statement Int32 (Maybe (Text, Text, Maybe Text))
selectUserDetails'' =
  [maybeStatement|
    select name :: text, email :: text, phone :: text?
    from user
    where ida = $1 :: int4
    |]
@
>>> tryPrepare conn selectUserDetails
Just (ResultError (ServerError "42703" "column \"ida\" does not exist" Nothing (Just "Perhaps you meant to reference the column \"user.id\".")))
-}
tryPrepare :: Connection -> Statement.Statement a b -> IO (Maybe CommandError)
tryPrepare (Connection pqConnectionRef _ registry) (Statement.Statement template _ _ _) =
  fmap (either Just (const Nothing))
    $ withMVar pqConnectionRef
    $ \pqConnection -> IO.getPreparedStatementKey pqConnection registry template []
