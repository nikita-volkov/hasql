module Hasql.Statement
  ( Statement,
    preparable,
    unpreparable,
    refineResult,
    toSql,

    -- * Recipes

    -- ** Insert many

    -- |
    -- Starting from PostgreSQL 9.4 there is an @unnest@ function which we can use in an analogous way
    -- to haskell's `zip` to pass in multiple arrays of values
    -- to be zipped into the rows to insert as in the following example:
    --
    -- @
    -- insertMultipleLocations :: 'Statement' (Vector (UUID, Double, Double)) ()
    -- insertMultipleLocations =
    --   'preparable' sql encoder decoder
    --   where
    --     sql =
    --       "insert into location (id, x, y) select * from unnest ($1, $2, $3)"
    --     encoder =
    --       Data.Vector.'Data.Vector.unzip3' '>$<'
    --         Contravariant.Extras.contrazip3
    --           (Encoders.'Encoders.param' $ Encoders.'Encoders.nonNullable' $ Encoders.'Encoders.foldableArray' $ Encoders.'Encoders.nonNullable' Encoders.'Encoders.uuid')
    --           (Encoders.'Encoders.param' $ Encoders.'Encoders.nonNullable' $ Encoders.'Encoders.foldableArray' $ Encoders.'Encoders.nonNullable' Encoders.'Encoders.float8')
    --           (Encoders.'Encoders.param' $ Encoders.'Encoders.nonNullable' $ Encoders.'Encoders.foldableArray' $ Encoders.'Encoders.nonNullable' Encoders.'Encoders.float8')
    --     decoder =
    --       Decoders.'Decoders.noResult'
    -- @
    --
    -- While this approach is much more efficient than executing a single-row insert-statement multiple times from within 'Session', a comparable performance can also be achieved by executing a single-insert statement from within a 'Pipeline'.

    -- ** IN and NOT IN

    -- |
    -- There is a common misconception that PostgreSQL supports array
    -- as the parameter for the @IN@ operator.
    -- However Postgres only supports a syntactical list of values with it,
    -- i.e., you have to specify each option as an individual parameter.
    -- E.g., @some_expression IN ($1, $2, $3)@.
    --
    -- Fortunately, Postgres does provide the expected functionality for arrays with other operators:
    --
    -- * Use @some_expression = ANY($1)@ instead of @some_expression IN ($1)@
    -- * Use @some_expression <> ALL($1)@ instead of @some_expression NOT IN ($1)@
    --
    -- For details refer to
    -- <https://www.postgresql.org/docs/9.6/static/functions-comparisons.html#AEN20944 the PostgreSQL docs>.
  )
where

import Hasql.Engine.Statement
