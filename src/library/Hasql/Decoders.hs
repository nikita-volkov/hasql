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
    namedComposite,
    hstore,
    enum,
    namedEnum,
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

import Core.Contexts.ArrayDecoder qualified as Array
import Core.Contexts.CompositeDecoder qualified as Composite
import Core.Contexts.ResultConsumer qualified as Result
import Core.Contexts.RowDecoder qualified as Row
import Core.Contexts.ValueDecoder qualified as Value
import Core.PostgresTypeInfo qualified as PTI
import Data.Aeson qualified as Aeson
import Data.IP qualified as Iproute
import Data.Vector.Generic qualified as GenericVector
import Platform.Prelude hiding (bool, maybe)
import PostgreSQL.Binary.Decoding qualified as A
import PostgreSQL.Binary.Range qualified as R

-- * Result

-- |
-- Decoder of a query result.
newtype Result a = Result (Result.ResultConsumer a) deriving (Functor, Filterable)

instance Result.Wraps Result where
  wrap = Result
  unwrap (Result r) = r

-- |
-- Decode no value from the result.
--
-- Useful for statements like @INSERT@ or @CREATE@.
{-# INLINEABLE noResult #-}
noResult :: Result ()
noResult = Result Result.noResult

-- |
-- Get the amount of rows affected by such statements as
-- @UPDATE@ or @DELETE@.
{-# INLINEABLE rowsAffected #-}
rowsAffected :: Result Int64
rowsAffected = Result Result.rowsAffected

-- |
-- Exactly one row.
-- Will raise the 'Errors.UnexpectedAmountOfRows' error if it's any other.
{-# INLINEABLE singleRow #-}
singleRow :: Row a -> Result a
singleRow (Row row) = Result (Result.single row)

refineResult :: (a -> Either Text b) -> Result a -> Result b
refineResult refiner (Result resultDecoder) = Result (Result.refine refiner resultDecoder)

-- ** Multi-row traversers

-- |
-- Foldl multiple rows.
{-# INLINEABLE foldlRows #-}
foldlRows :: (a -> b -> a) -> a -> Row b -> Result a
foldlRows step init (Row row) = Result (Result.foldl step init row)

-- |
-- Foldr multiple rows.
{-# INLINEABLE foldrRows #-}
foldrRows :: (b -> a -> a) -> a -> Row b -> Result a
foldrRows step init (Row row) = Result (Result.foldr step init row)

-- ** Specialized multi-row results

-- |
-- Maybe one row or none.
{-# INLINEABLE rowMaybe #-}
rowMaybe :: Row a -> Result (Maybe a)
rowMaybe (Row row) = Result (Result.maybe row)

-- |
-- Zero or more rows packed into the vector.
--
-- It's recommended to prefer this function to 'rowList',
-- since it performs notably better.
{-# INLINEABLE rowVector #-}
rowVector :: Row a -> Result (Vector a)
rowVector (Row row) = Result (Result.vector row)

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
newtype Row a = Row (Row.RowDecoder a)
  deriving (Functor, Applicative)

-- |
-- Lift an individual value decoder to a composable row decoder.
{-# INLINEABLE column #-}
column :: NullableOrNot Value a -> Row a
column = \case
  NonNullable (Value imp) -> Row (Row.nonNullableColumn imp)
  Nullable (Value imp) -> Row (Row.nullableColumn imp)

-- * Nullability

-- |
-- Extensional specification of nullability over a generic decoder.
data NullableOrNot decoder a where
  NonNullable :: decoder a -> NullableOrNot decoder a
  Nullable :: decoder a -> NullableOrNot decoder (Maybe a)

-- |
-- Specify that a decoder produces a non-nullable value.
nonNullable :: decoder a -> NullableOrNot decoder a
nonNullable = NonNullable

-- |
-- Specify that a decoder produces a nullable value.
nullable :: decoder a -> NullableOrNot decoder (Maybe a)
nullable = Nullable

-- * Value

-- |
-- Decoder of a value.
newtype Value a = Value (Value.ValueDecoder a)
  deriving (Functor, Filterable)

type role Value representational

-- |
-- Decoder of the @BOOL@ values.
{-# INLINEABLE bool #-}
bool :: Value Bool
bool = Value (Value.unsafePTI "bool" PTI.bool A.bool)

-- |
-- Decoder of the @INT2@ values.
{-# INLINEABLE int2 #-}
int2 :: Value Int16
int2 = Value (Value.unsafePTI "int2" PTI.int2 A.int)

-- |
-- Decoder of the @INT4@ values.
{-# INLINEABLE int4 #-}
int4 :: Value Int32
int4 = Value (Value.unsafePTI "int4" PTI.int4 A.int)

-- |
-- Decoder of the @INT8@ values.
{-# INLINEABLE int8 #-}
int8 :: Value Int64
int8 =
  {-# SCC "int8" #-}
  Value (Value.unsafePTI "int8" PTI.int8 ({-# SCC "int8.int" #-} A.int))

-- |
-- Decoder of the @FLOAT4@ values.
{-# INLINEABLE float4 #-}
float4 :: Value Float
float4 = Value (Value.unsafePTI "float4" PTI.float4 A.float4)

-- |
-- Decoder of the @FLOAT8@ values.
{-# INLINEABLE float8 #-}
float8 :: Value Double
float8 = Value (Value.unsafePTI "float8" PTI.float8 A.float8)

-- |
-- Decoder of the @NUMERIC@ values.
{-# INLINEABLE numeric #-}
numeric :: Value Scientific
numeric = Value (Value.unsafePTI "numeric" PTI.numeric A.numeric)

-- |
-- Decoder of the @CHAR@ values.
-- Note that it supports Unicode values.
{-# INLINEABLE char #-}
char :: Value Char
char = Value (Value.unsafePTI "char" PTI.char A.char)

-- |
-- Decoder of the @TEXT@ values.
{-# INLINEABLE text #-}
text :: Value Text
text = Value (Value.unsafePTI "text" PTI.text A.text_strict)

-- |
-- Decoder of the @BYTEA@ values.
{-# INLINEABLE bytea #-}
bytea :: Value ByteString
bytea = Value (Value.unsafePTI "bytea" PTI.bytea A.bytea_strict)

-- |
-- Decoder of the @DATE@ values.
{-# INLINEABLE date #-}
date :: Value Day
date = Value (Value.unsafePTI "date" PTI.date A.date)

-- |
-- Decoder of the @TIMESTAMP@ values.
{-# INLINEABLE timestamp #-}
timestamp :: Value LocalTime
timestamp = Value (Value.unsafePTI "timestamp" PTI.timestamp A.timestamp_int)

-- |
-- Decoder of the @TIMESTAMPTZ@ values.
--
-- /NOTICE/
--
-- Postgres does not store the timezone information of @TIMESTAMPTZ@.
-- Instead it stores a UTC value and performs silent conversions
-- to the currently set timezone, when dealt with in the text format.
-- However this library bypasses the silent conversions
-- and communicates with Postgres using the UTC values directly.
{-# INLINEABLE timestamptz #-}
timestamptz :: Value UTCTime
timestamptz = Value (Value.unsafePTI "timestamptz" PTI.timestamptz A.timestamptz_int)

-- |
-- Decoder of the @TIME@ values.
{-# INLINEABLE time #-}
time :: Value TimeOfDay
time = Value (Value.unsafePTI "time" PTI.time A.time_int)

-- |
-- Decoder of the @TIMETZ@ values.
--
-- Unlike in case of @TIMESTAMPTZ@,
-- Postgres does store the timezone information for @TIMETZ@.
-- However the Haskell's \"time\" library does not contain any composite type,
-- that fits the task, so we use a pair of 'TimeOfDay' and 'TimeZone'
-- to represent a value on the Haskell's side.
{-# INLINEABLE timetz #-}
timetz :: Value (TimeOfDay, TimeZone)
timetz = Value (Value.unsafePTI "timetz" PTI.timetz A.timetz_int)

-- |
-- Decoder of the @INTERVAL@ values.
{-# INLINEABLE interval #-}
interval :: Value DiffTime
interval = Value (Value.unsafePTI "interval" PTI.interval A.interval_int)

-- |
-- Decoder of the @UUID@ values.
{-# INLINEABLE uuid #-}
uuid :: Value UUID
uuid = Value (Value.unsafePTI "uuid" PTI.uuid A.uuid)

-- |
-- Decoder of the @INET@ values.
{-# INLINEABLE inet #-}
inet :: Value Iproute.IPRange
inet = Value (Value.unsafePTI "inet" PTI.inet A.inet)

-- |
-- Decoder of the @MACADDR@ values.
--
-- Represented as a 6-tuple of Word8 values in big endian order. If
-- you use `ip` library consider using it with `fromOctets`.
--
-- > (\(a,b,c,d,e,f) -> fromOctets a b c d e f) <$> macaddr
{-# INLINEABLE macaddr #-}
macaddr :: Value (Word8, Word8, Word8, Word8, Word8, Word8)
macaddr = Value (Value.unsafePTI "macaddr" PTI.macaddr A.macaddr)

-- |
-- Decoder of the @JSON@ values into a JSON AST.
{-# INLINEABLE json #-}
json :: Value Aeson.Value
json = Value (Value.unsafePTI "json" PTI.json A.json_ast)

-- |
-- Decoder of the @JSON@ values into a raw JSON 'ByteString'.
{-# INLINEABLE jsonBytes #-}
jsonBytes :: (ByteString -> Either Text a) -> Value a
jsonBytes fn = Value (Value.decoder (A.json_bytes fn))

-- |
-- Decoder of the @JSONB@ values into a JSON AST.
{-# INLINEABLE jsonb #-}
jsonb :: Value Aeson.Value
jsonb = Value (Value.unsafePTI "jsonb" PTI.jsonb A.jsonb_ast)

-- |
-- Decoder of the @JSONB@ values into a raw JSON 'ByteString'.
{-# INLINEABLE jsonbBytes #-}
jsonbBytes :: (ByteString -> Either Text a) -> Value a
jsonbBytes fn = Value (Value.decoder (A.jsonb_bytes fn))

-- |
-- Decoder of the @INT4RANGE@ values.
{-# INLINEABLE int4range #-}
int4range :: Value (R.Range Int32)
int4range = Value (Value.unsafePTI "int4range" PTI.int4range A.int4range)

-- |
-- Decoder of the @INT8RANGE@ values.
{-# INLINEABLE int8range #-}
int8range :: Value (R.Range Int64)
int8range = Value (Value.unsafePTI "int8range" PTI.int8range A.int8range)

-- |
-- Decoder of the @NUMRANGE@ values.
{-# INLINEABLE numrange #-}
numrange :: Value (R.Range Scientific)
numrange = Value (Value.unsafePTI "numrange" PTI.numrange A.numrange)

-- |
-- Decoder of the @TSRANGE@ values.
{-# INLINEABLE tsrange #-}
tsrange :: Value (R.Range LocalTime)
tsrange = Value (Value.unsafePTI "tsrange" PTI.tsrange A.tsrange_int)

-- |
-- Decoder of the @TSTZRANGE@ values.
{-# INLINEABLE tstzrange #-}
tstzrange :: Value (R.Range UTCTime)
tstzrange = Value (Value.unsafePTI "tstzrange" PTI.tstzrange A.tstzrange_int)

-- |
-- Decoder of the @DATERANGE@ values.
{-# INLINEABLE daterange #-}
daterange :: Value (R.Range Day)
daterange = Value (Value.unsafePTI "daterange" PTI.daterange A.daterange)

-- |
-- Decoder of the @INT4MULTIRANGE@ values.
{-# INLINEABLE int4multirange #-}
int4multirange :: Value (R.Multirange Int32)
int4multirange = Value (Value.unsafePTI "int4multirange" PTI.int4multirange A.int4multirange)

-- |
-- Decoder of the @INT8MULTIRANGE@ values.
{-# INLINEABLE int8multirange #-}
int8multirange :: Value (R.Multirange Int64)
int8multirange = Value (Value.unsafePTI "int8multirange" PTI.int8multirange A.int8multirange)

-- |
-- Decoder of the @NUMMULTIRANGE@ values.
{-# INLINEABLE nummultirange #-}
nummultirange :: Value (R.Multirange Scientific)
nummultirange = Value (Value.unsafePTI "nummultirange" PTI.nummultirange A.nummultirange)

-- |
-- Decoder of the @TSMULTIRANGE@ values.
{-# INLINEABLE tsmultirange #-}
tsmultirange :: Value (R.Multirange LocalTime)
tsmultirange = Value (Value.unsafePTI "tsmultirange" PTI.tsmultirange A.tsmultirange_int)

-- |
-- Decoder of the @TSTZMULTIRANGE@ values.
{-# INLINEABLE tstzmultirange #-}
tstzmultirange :: Value (R.Multirange UTCTime)
tstzmultirange = Value (Value.unsafePTI "tstzmultirange" PTI.tstzmultirange A.tstzmultirange_int)

-- |
-- Decoder of the @DATEMULTIRANGE@ values.
{-# INLINEABLE datemultirange #-}
datemultirange :: Value (R.Multirange Day)
datemultirange = Value (Value.unsafePTI "datemultirange" PTI.datemultirange A.datemultirange)

-- |
-- Lift a custom value decoder function to a 'Value' decoder.
{-# INLINEABLE custom #-}
custom :: (ByteString -> Either Text a) -> Value a
custom fn = Value (Value.decoder (A.fn fn))

-- |
-- Refine a value decoder, lifting the possible error to the session level.
{-# INLINEABLE refine #-}
refine :: (a -> Either Text b) -> Value a -> Value b
refine fn (Value v) = Value (Value.refine fn v)

-- |
-- A generic decoder of @HSTORE@ values.
--
-- Here's how you can use it to construct a specific value:
--
-- @
-- x :: Value [(Text, Maybe Text)]
-- x = hstore 'replicateM'
-- @
{-# INLINEABLE hstore #-}
hstore :: (forall m. (Monad m) => Int -> m (Text, Maybe Text) -> m a) -> Value a
hstore replicateM = Value (Value.decoder (A.hstore replicateM A.text_strict A.text_strict))

-- |
-- Given a partial mapping from text to value,
-- produces a decoder of that value.
enum :: (Text -> Maybe a) -> Value a
enum mapping = Value (Value.decoder (A.enum mapping))

-- |
-- Lift an 'Array' decoder to a 'Value' decoder.
{-# INLINEABLE array #-}
array :: Array a -> Value a
array (Array decoder) = Value (Value.ValueDecoder (Array.toTypeName decoder) (Array.toOid decoder) Nothing (Array.toDecoder decoder))

-- |
-- Lift a value decoder of element into a unidimensional array decoder producing a list.
--
-- This function is merely a shortcut to the following expression:
--
-- @
-- ('array' . 'dimension' Control.Monad.'replicateM' . 'element')
-- @
--
-- Please notice that in case of multidimensional arrays nesting 'listArray' decoder
-- won't work. You have to explicitly construct the array decoder using 'array'.
{-# INLINE listArray #-}
listArray :: NullableOrNot Value element -> Value [element]
listArray = array . dimension replicateM . element

-- |
-- Lift a value decoder of element into a unidimensional array decoder producing a generic vector.
--
-- This function is merely a shortcut to the following expression:
--
-- @
-- ('array' . 'dimension' Data.Vector.Generic.'GenericVector.replicateM' . 'element')
-- @
--
-- Please notice that in case of multidimensional arrays nesting 'vectorArray' decoder
-- won't work. You have to explicitly construct the array decoder using 'array'.
{-# INLINE vectorArray #-}
vectorArray :: (GenericVector.Vector vector element) => NullableOrNot Value element -> Value (vector element)
vectorArray = array . dimension GenericVector.replicateM . element

-- |
-- Lift a 'Composite' decoder to a 'Value' decoder.
{-# INLINEABLE composite #-}
composite :: Composite a -> Value a
composite (Composite imp) = Value (Value.ValueDecoder "unknown" Nothing Nothing (Composite.run imp))

-- |
-- Decoder for a named composite type.
-- The type name will be resolved to an OID at runtime.
{-# INLINEABLE namedComposite #-}
namedComposite :: Text -> Composite a -> Value a
namedComposite typeName (Composite imp) = Value (Value.ValueDecoder typeName Nothing Nothing (Composite.run imp))

-- |
-- Decoder for a named enum type.
-- The type name will be resolved to an OID at runtime.
{-# INLINEABLE namedEnum #-}
namedEnum :: Text -> (Text -> Maybe a) -> Value a
namedEnum typeName mapping = Value (Value.ValueDecoder typeName Nothing Nothing (A.enum mapping))

-- * Array decoders

-- |
-- A generic array decoder.
--
-- Here's how you can use it to produce a specific array value decoder:
--
-- @
-- x :: 'Value' [[Text]]
-- x = 'array' ('dimension' 'replicateM' ('dimension' 'replicateM' ('element' ('nonNullable' 'text'))))
-- @
newtype Array a = Array (Array.ArrayDecoder a)
  deriving (Functor)

-- |
-- A function for parsing a dimension of an array.
-- Provides support for multi-dimensional arrays.
--
-- Accepts:
--
-- * An implementation of the @replicateM@ function
-- (@Control.Monad.'Control.Monad.replicateM'@, @Data.Vector.'Data.Vector.replicateM'@),
-- which determines the output value.
--
-- * A decoder of its components, which can be either another 'dimension' or 'element'.
{-# INLINEABLE dimension #-}
dimension :: (forall m. (Monad m) => Int -> m a -> m b) -> Array a -> Array b
dimension replicateM (Array imp) = Array (Array.dimension replicateM imp)

-- |
-- Lift a 'Value' decoder into an 'Array' decoder for parsing of leaf values.
{-# INLINEABLE element #-}
element :: NullableOrNot Value a -> Array a
element = \case
  NonNullable (Value imp) ->
    Array
      ( Array.nonNullableElement
          (Value.toTypeName imp)
          (Value.toArrayOid imp)
          (Value.toHandler imp)
      )
  Nullable (Value imp) ->
    Array
      ( Array.nullableElement
          (Value.toTypeName imp)
          (Value.toArrayOid imp)
          (Value.toHandler imp)
      )

-- * Composite decoders

-- |
-- Composable decoder of composite values (rows, records).
newtype Composite a = Composite (Composite.CompositeDecoder a)
  deriving (Functor, Applicative, Monad, MonadFail)

-- |
-- Lift a 'Value' decoder into a 'Composite' decoder for parsing of component values.
field :: NullableOrNot Value a -> Composite a
field = \case
  NonNullable (Value imp) -> Composite (Composite.nonNullValue (Value.toHandler imp))
  Nullable (Value imp) -> Composite (Composite.value (Value.toHandler imp))
