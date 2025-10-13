-- |
-- A DSL for declaration of result decoders.
module Hasql.Decoders
  ( -- * Result
    Result,
    noResult,
    rowsAffected,
    singleRow,
    refineResult,

    -- ** Specialized multi-row results
    rowMaybe,
    rowVector,
    rowList,

    -- ** Multi-row traversers
    foldlRows,
    foldrRows,

    -- * Row
    Row,
    column,

    -- * Nullability
    NullableOrNot,
    nonNullable,
    nullable,

    -- * Value
    Value,
    bool,
    int2,
    int4,
    int8,
    float4,
    float8,
    numeric,
    char,
    text,
    bytea,
    date,
    timestamp,
    timestamptz,
    time,
    timetz,
    interval,
    uuid,
    inet,
    macaddr,
    json,
    jsonBytes,
    jsonb,
    jsonbBytes,
    int4range,
    int8range,
    numrange,
    tsrange,
    tstzrange,
    daterange,
    int4multirange,
    int8multirange,
    nummultirange,
    tsmultirange,
    tstzmultirange,
    datemultirange,
    array,
    listArray,
    vectorArray,
    composite,
    hstore,
    enum,
    custom,
    refine,

    -- * Array
    Array,
    dimension,
    element,

    -- * Composite
    Composite,
    field,
  )
where

import Codecs.Decoders
import Codecs.Decoders.Value qualified as Value
import Hipq.ResultDecoder qualified as ResultDecoder
import Hipq.RowDecoder qualified as RowDecoder
import Platform.Prelude hiding (bool, maybe)

-- * Result

-- |
-- Decoder of a query result.
newtype Result a = Result (ResultDecoder.ResultDecoder a)
  deriving newtype (Functor, Filterable)

instance ResultDecoder.Wraps Result where
  wrap = Result
  unwrap (Result r) = r

-- |
-- Decode no value from the result.
--
-- Useful for statements like @INSERT@ or @CREATE@.
{-# INLINEABLE noResult #-}
noResult :: Result ()
noResult = Result ResultDecoder.ok

-- |
-- Get the amount of rows affected by such statements as
-- @UPDATE@ or @DELETE@.
{-# INLINEABLE rowsAffected #-}
rowsAffected :: Result Int64
rowsAffected = Result ResultDecoder.rowsAffected

-- |
-- Exactly one row.
-- Will raise the 'Errors.UnexpectedAmountOfRows' error if it's any other.
{-# INLINEABLE singleRow #-}
singleRow :: Row a -> Result a
singleRow (Row row) = Result (ResultDecoder.single row)

refineResult :: (a -> Either Text b) -> Result a -> Result b
refineResult refiner (Result resultDecoder) = Result (ResultDecoder.refine refiner resultDecoder)

-- ** Multi-row traversers

-- |
-- Foldl multiple rows.
{-# INLINEABLE foldlRows #-}
foldlRows :: (a -> b -> a) -> a -> Row b -> Result a
foldlRows step init (Row row) = Result (ResultDecoder.foldl step init row)

-- |
-- Foldr multiple rows.
{-# INLINEABLE foldrRows #-}
foldrRows :: (b -> a -> a) -> a -> Row b -> Result a
foldrRows step init (Row row) = Result (ResultDecoder.foldr step init row)

-- ** Specialized multi-row results

-- |
-- Maybe one row or none.
{-# INLINEABLE rowMaybe #-}
rowMaybe :: Row a -> Result (Maybe a)
rowMaybe (Row row) = Result (ResultDecoder.maybe row)

-- |
-- Zero or more rows packed into the vector.
--
-- It's recommended to prefer this function to 'rowList',
-- since it performs notably better.
{-# INLINEABLE rowVector #-}
rowVector :: Row a -> Result (Vector a)
rowVector (Row row) = Result (ResultDecoder.vector row)

-- |
-- Zero or more rows packed into the list.
{-# INLINEABLE rowList #-}
rowList :: Row a -> Result [a]
rowList = foldrRows strictCons []

-- * Row

-- |
-- Decoder of an individual row,
-- which gets composed of column value decoders.
--
-- E.g.:
--
-- @
-- x :: 'Row' (Maybe Int64, Text, TimeOfDay)
-- x = (,,) '<$>' ('column' . 'nullable') 'int8' '<*>' ('column' . 'nonNullable') 'text' '<*>' ('column' . 'nonNullable') 'time'
-- @
newtype Row a = Row (RowDecoder.RowDecoder a)
  deriving newtype (Functor, Applicative)

-- |
-- Lift an individual value decoder to a composable row decoder.
{-# INLINEABLE column #-}
column :: NullableOrNot Value a -> Row a
column = \case
  Nullable valueDecoder ->
    Row
      ( RowDecoder.nullableColumn
          (Value.toBaseOid valueDecoder)
          (Value.toByteStringParser valueDecoder)
      )
  NonNullable valueDecoder ->
    Row
      ( RowDecoder.nonNullableColumn
          (Value.toBaseOid valueDecoder)
          (Value.toByteStringParser valueDecoder)
      )
