module Hasql.Statement
  ( Statement (Statement),
    preparable,
    unpreparable,
    refineResult,

    -- * Recipes

    -- ** Insert many
    -- $insertMany

    -- ** IN and NOT IN
    -- $inAndNotIn
  )
where

import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Engine.Decoders.Result qualified
import Hasql.Platform.Prelude

-- |
-- Specification of a strictly single-statement query, which can be parameterized and prepared.
-- It encapsulates the mapping of parameters and results in association with an SQL template.
--
-- Following is an example of a declaration of a prepared statement with its associated codecs.
--
-- @
-- selectSum :: 'Statement' (Int64, Int64) Int64
-- selectSum =
--   'preparable' sql encoder decoder
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
data Statement params result
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
      (Encoders.Params params)
      -- | Decoder of result.
      (Decoders.Result result)
      -- | Flag, determining whether it can be prepared.
      --
      -- Set it to 'True' if your application has a limited amount of queries and doesn't generate the SQL dynamically.
      -- This will boost the performance by allowing Postgres to avoid reconstructing the execution plan each time the query gets executed.
      Bool

{-# DEPRECATED Statement "Use 'preparable' or 'unpreparable' instead of the Statement constructor" #-}

-- |
-- Construct a preparable statement.
--
-- Use this for statements that will be executed multiple times with different parameters.
-- Preparable statements are cached by PostgreSQL, which avoids reconstructing the execution plan each time.
--
-- Suitable for applications with a limited amount of queries that don't generate SQL dynamically.
preparable ::
  -- | SQL template with parameters in positional notation (@$1@, @$2@, etc.)
  ByteString ->
  -- | Parameters encoder
  Encoders.Params params ->
  -- | Result decoder
  Decoders.Result result ->
  Statement params result
preparable sql encoder decoder = Statement sql encoder decoder True

-- |
-- Construct an unpreparable statement.
--
-- Use this for statements that are dynamically generated or executed only once.
-- Unpreparable statements are not cached by PostgreSQL.
--
-- Suitable for dynamic SQL or one-off queries.
unpreparable ::
  -- | SQL template with parameters in positional notation (@$1@, @$2@, etc.)
  ByteString ->
  -- | Parameters encoder
  Encoders.Params params ->
  -- | Result decoder
  Decoders.Result result ->
  Statement params result
unpreparable sql encoder decoder = Statement sql encoder decoder False

instance Functor (Statement params) where
  {-# INLINE fmap #-}
  fmap = rmap

instance Filterable (Statement params) where
  {-# INLINE mapMaybe #-}
  mapMaybe filtrator (Statement template encoder decoder preparable) =
    Statement template encoder (mapMaybe filtrator decoder) preparable

instance Profunctor Statement where
  {-# INLINE dimap #-}
  dimap f1 f2 (Statement template encoder decoder preparable) =
    Statement template (contramap f1 encoder) (fmap f2 decoder) preparable

-- |
-- Refine the result of a statement,
-- causing the running session to fail with the 'Hasql.Errors.UnexpectedResultStatementError' error in case of a refinement failure.
--
-- This function is especially useful for refining the results of statements produced with
-- <http://hackage.haskell.org/package/hasql-th the \"hasql-th\" library>.
refineResult :: (a -> Either Text b) -> Statement params a -> Statement params b
refineResult refiner (Statement template encoder decoder preparable) =
  Statement template encoder (Hasql.Engine.Decoders.Result.refineResult refiner decoder) preparable

-- $insertMany
--
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
-- * Use @some_expression = ANY($1)@ instead of @some_expression IN ($1)@
-- * Use @some_expression <> ALL($1)@ instead of @some_expression NOT IN ($1)@
--
-- For details refer to
-- <https://www.postgresql.org/docs/9.6/static/functions-comparisons.html#AEN20944 the PostgreSQL docs>.
