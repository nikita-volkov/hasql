-- |
-- A DSL for declaration of statement parameter encoders.
--
-- For compactness of names all the types defined here imply being an encoder.
-- E.g., the `Array` type is an __encoder__ of arrays, not the data-structure itself.
module Hasql.Encoders
  ( -- * Parameters product
    Params,
    noParams,
    param,

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
    jsonLazyBytes,
    jsonb,
    jsonbBytes,
    jsonbLazyBytes,
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
    name,
    oid,
    foldableArray,
    array,
    hstore,
    enum,
    composite,
    custom,
    unknown,

    -- * Array
    Array,
    element,
    dimension,

    -- * Composite
    Composite,
    field,
  )
where

-- Every identifier below is imported directly from the module it's
-- actually declared in, rather than from "Hasql.Codecs.Encoders" (which
-- merely re-exports them). Haddock has a bug where docs of an identifier
-- re-exported through more than one hop (declaration site -> intermediate
-- re-export -> here) silently render blank on Hackage; importing straight
-- from the declaration site keeps this a single hop.
import Hasql.Codecs.Encoders (array, composite, foldableArray)
import Hasql.Codecs.Encoders.Array (Array, dimension, element)
import Hasql.Codecs.Encoders.Composite (Composite, field)
import Hasql.Codecs.Encoders.NullableOrNot (NullableOrNot, nonNullable, nullable)
import Hasql.Codecs.Encoders.Params (Params, noParams, param)
import Hasql.Codecs.Encoders.Value
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
    jsonLazyBytes,
    jsonb,
    jsonbBytes,
    jsonbLazyBytes,
    macaddr,
    name,
    numeric,
    nummultirange,
    numrange,
    oid,
    text,
    time,
    timestamp,
    timestamptz,
    timetz,
    tsmultirange,
    tsrange,
    tstzmultirange,
    tstzrange,
    unknown,
    uuid,
    varchar,
  )
