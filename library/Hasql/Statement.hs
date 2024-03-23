module Hasql.Statement
  ( Statement (..),
    refineResult,

    -- * Recipes

    -- ** Insert many
    -- $insertMany

    -- ** IN and NOT IN
    -- $inAndNotIn
  )
where

import qualified Hasql.Decoders as Decoders
import qualified Hasql.Decoders.All as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Prelude

-- |
-- Specification of a strictly single-statement query, which can be parameterized and prepared, encapsulating the mapping of parameters and results.
--
-- Following is an example of a declaration of a prepared statement with its associated codecs.
--
-- @
-- selectSum :: 'Statement' (Int64, Int64) Int64
-- selectSum =
--   'Statement' sql encoder decoder True
--   where
--     sql =
--       \"select ($1 + $2)\"
--     encoder =
--       ('fst' '>$<' Encoders.'Hasql.Encoders.param' (Encoders.'Hasql.Encoders.nonNullable' Encoders.'Hasql.Encoders.int8')) '<>'
--       ('snd' '>$<' Encoders.'Hasql.Encoders.param' (Encoders.'Hasql.Encoders.nonNullable' Encoders.'Hasql.Encoders.int8'))
--     decoder =
--       Decoders.'Hasql.Decoders.singleRow' (Decoders.'Hasql.Decoders.column' (Decoders.'Hasql.Decoders.nonNullable' Decoders.'Hasql.Decoders.int8'))
-- @
--
-- The statement above accepts a product of two parameters of type 'Int64'
-- and produces a single result of type 'Int64'.
data Statement a b
  = Statement
      -- | SQL template.
      --
      -- Must be formatted according to the Postgres standard,
      -- with any non-ASCII characters of the template encoded using UTF-8.
      -- The parameters must be referred to using the positional notation, as in the following:
      -- @$1@, @$2@, @$3@ and etc.
      -- These references must be used in accordance with the order in which
      -- the value encoders are specified in the parameters encoder.
      ByteString
      -- | Parameters encoder.
      (Encoders.Params a)
      -- | Decoder of result.
      (Decoders.Result b)
      -- | Flag, determining whether it should be prepared.
      --
      -- Set it to 'True' if your application has a limited amount of queries and doesn't generate the SQL dynamically.
      -- This will boost the performance by allowing Postgres to avoid reconstructing the execution plan each time the query gets executed.
      --
      -- Note that if you're using proxying applications like @pgbouncer@, such tools may be incompatible with prepared statements.
      -- So do consult their docs or just set it to 'False' to stay on the safe side.
      -- It should be noted that starting from version @1.21.0@ @pgbouncer@ now does provide support for prepared statements.
      Bool

instance Functor (Statement a) where
  {-# INLINE fmap #-}
  fmap = rmap

instance Profunctor Statement where
  {-# INLINE dimap #-}
  dimap f1 f2 (Statement template encoder decoder preparable) =
    Statement template (contramap f1 encoder) (fmap f2 decoder) preparable

-- |
-- Refine the result of a statement,
-- causing the running session to fail with the `UnexpectedResult` error in case of a refinement failure.
--
-- This function is especially useful for refining the results of statements produced with
-- <http://hackage.haskell.org/package/hasql-th the \"hasql-th\" library>.
refineResult :: (a -> Either Text b) -> Statement params a -> Statement params b
refineResult refiner (Statement template encoder decoder preparable) =
  Statement template encoder (Decoders.refineResult refiner decoder) preparable

-- $insertMany
--
-- Starting from PostgreSQL 9.4 there is an @unnest@ function which we can use in an analogous way
-- to haskell's `zip` to pass in multiple arrays of values
-- to be zipped into the rows to insert as in the following example:
--
-- @
-- insertMultipleLocations :: 'Statement' (Vector (UUID, Double, Double)) ()
-- insertMultipleLocations =
--   'Statement' sql encoder decoder True
--   where
--     sql =
--       "insert into location (id, x, y) select * from unnest ($1, $2, $3)"
--     encoder =
--       Data.Vector.'Data.Vector.unzip3' '>$<'
--         Contravariant.Extras.'Contravariant.Extras.contrazip3'
--           (Encoders.'Encoders.param' $ Encoders.'Encoders.nonNullable' $ Encoders.'Encoders.foldableArray' $ Encoders.'Encoders.nonNullable' Encoders.'Encoders.uuid')
--           (Encoders.'Encoders.param' $ Encoders.'Encoders.nonNullable' $ Encoders.'Encoders.foldableArray' $ Encoders.'Encoders.nonNullable' Encoders.'Encoders.float8')
--           (Encoders.'Encoders.param' $ Encoders.'Encoders.nonNullable' $ Encoders.'Encoders.foldableArray' $ Encoders.'Encoders.nonNullable' Encoders.'Encoders.float8')
--     decoder =
--       Decoders.'Decoders.noResult'
-- @
--
-- This approach is much more efficient than executing a single-row insert-statement multiple times.

-- $inAndNotIn
--
-- There is a common misconception that PostgreSQL supports array
-- as the parameter for the @IN@ operator.
-- However Postgres only supports a syntactical list of values with it,
-- i.e., you have to specify each option as an individual parameter.
-- E.g., @some_expression IN ($1, $2, $3)@.
--
-- Fortunately, Postgres does provide the expected functionality for arrays with other operators:
--
-- * Use @something = ANY($1)@ instead of @something IN ($1)@
-- * Use @something <> ALL($1)@ instead of @something NOT IN ($1)@
--
-- For details refer to
-- <https://www.postgresql.org/docs/9.6/static/functions-comparisons.html#AEN20944 the PostgreSQL docs>.
