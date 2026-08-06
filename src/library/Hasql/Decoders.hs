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
    varchar,
    bpchar,
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
    citext,
    array,
    listArray,
    vectorArray,
    composite,
    record,
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

-- Every identifier below is imported directly from the module it's
-- actually declared in, rather than from an intermediate re-export
-- module. Haddock has a bug where docs of an identifier re-exported
-- through more than one hop (declaration site -> intermediate re-export
-- -> here) silently render blank on Hackage; importing straight from the
-- declaration site keeps this a single hop.
import Hasql.Codecs.Decoders (array, composite, listArray, record, vectorArray)
import Hasql.Codecs.Decoders.Array (Array, dimension, element)
import Hasql.Codecs.Decoders.Composite (Composite, field)
import Hasql.Codecs.Decoders.NullableOrNot (NullableOrNot, nonNullable, nullable)
import Hasql.Codecs.Decoders.Value
  ( Value,
    bool,
    bpchar,
    bytea,
    char,
    citext,
    custom,
    date,
    datemultirange,
    daterange,
    enum,
    float4,
    float8,
    hstore,
    inet,
    int2,
    int4,
    int4multirange,
    int4range,
    int8,
    int8multirange,
    int8range,
    interval,
    json,
    jsonBytes,
    jsonb,
    jsonbBytes,
    macaddr,
    numeric,
    nummultirange,
    numrange,
    refine,
    text,
    time,
    timestamp,
    timestamptz,
    timetz,
    tsmultirange,
    tsrange,
    tstzmultirange,
    tstzrange,
    uuid,
    varchar,
  )
import Hasql.Engine.Decoders.Result (Result, foldlRows, foldrRows, noResult, rowList, rowMaybe, rowVector, rowsAffected, singleRow)
import Hasql.Engine.Decoders.Row (Row, column)
