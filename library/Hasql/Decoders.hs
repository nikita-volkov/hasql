-- |
-- A DSL for declaration of result decoders.
module Hasql.Decoders
  ( -- * Result
    Result,
    noResult,
    rowsAffected,
    singleRow,

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
    json,
    jsonBytes,
    jsonb,
    jsonbBytes,
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

import Hasql.Decoders.All
