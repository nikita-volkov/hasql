{-|
A DSL for declaration of result decoders.
-}
module Hasql.Private.Decoders
where

import Hasql.Private.Prelude hiding (maybe, bool)
import qualified Data.Vector as Vector
import qualified PostgreSQL.Binary.Decoding as A
import qualified PostgreSQL.Binary.Data as B
import qualified Hasql.Private.Decoders.Results as Results
import qualified Hasql.Private.Decoders.Result as Result
import qualified Hasql.Private.Decoders.Row as Row
import qualified Hasql.Private.Decoders.Value as Value
import qualified Hasql.Private.Decoders.Array as Array
import qualified Hasql.Private.Decoders.Composite as Composite
import qualified Hasql.Private.Errors as Errors
import qualified Hasql.Private.Prelude as Prelude
import qualified Data.Vector.Generic as GenericVector

-- * Result
-------------------------

{-|
Decoder of a query result.
-}
newtype Result a = Result (Results.Results a) deriving (Functor)

{-|
Decode no value from the result.

Useful for statements like @INSERT@ or @CREATE@.
-}
{-# INLINABLE noResult #-}
noResult :: Result ()
noResult = Result (Results.single Result.noResult)

{-|
Get the amount of rows affected by such statements as
@UPDATE@ or @DELETE@.
-}
{-# INLINABLE rowsAffected #-}
rowsAffected :: Result Int64
rowsAffected = Result (Results.single Result.rowsAffected)

{-|
Exactly one row.
Will raise the 'Errors.UnexpectedAmountOfRows' error if it's any other.
-}
{-# INLINABLE singleRow #-}
singleRow :: Row a -> Result a
singleRow (Row row) = Result (Results.single (Result.single row))

refineResult :: (a -> Either Text b) -> Result a -> Result b
refineResult refiner (Result results) = Result (Results.refine refiner results)

-- ** Multi-row traversers
-------------------------

{-|
Foldl multiple rows.
-}
{-# INLINABLE foldlRows #-}
foldlRows :: (a -> b -> a) -> a -> Row b -> Result a
foldlRows step init (Row row) = Result (Results.single (Result.foldl step init row))

{-|
Foldr multiple rows.
-}
{-# INLINABLE foldrRows #-}
foldrRows :: (b -> a -> a) -> a -> Row b -> Result a
foldrRows step init (Row row) = Result (Results.single (Result.foldr step init row))

-- ** Specialized multi-row results
-------------------------

{-|
Maybe one row or none.
-}
{-# INLINABLE rowMaybe #-}
rowMaybe :: Row a -> Result (Maybe a)
rowMaybe (Row row) = Result (Results.single (Result.maybe row))

{-|
Zero or more rows packed into the vector.

It's recommended to prefer this function to 'rowList',
since it performs notably better.
-}
{-# INLINABLE rowVector #-}
rowVector :: Row a -> Result (Vector a)
rowVector (Row row) = Result (Results.single (Result.vector row))

{-|
Zero or more rows packed into the list.
-}
{-# INLINABLE rowList #-}
rowList :: Row a -> Result [a]
rowList = foldrRows strictCons []


-- * Row
-------------------------

{-|
Decoder of an individual row,
which gets composed of column value decoders.

E.g.:

@
x :: 'Row' (Maybe Int64, Text, TimeOfDay)
x = (,,) '<$>' ('column' . 'nullable') 'int8' '<*>' ('column' . 'nonNullable') 'text' '<*>' ('column' . 'nonNullable') 'time'
@
-}
newtype Row a = Row (Row.Row a)
  deriving (Functor, Applicative, Monad, MonadFail)

{-|
Lift an individual value decoder to a composable row decoder.
-}
{-# INLINABLE column #-}
column :: NullableOrNot Value a -> Row a
column = \ case
  NonNullable (Value imp) -> Row (Row.nonNullValue imp)
  Nullable (Value imp) -> Row (Row.value imp)


-- * Nullability
-------------------------

{-|
Extensional specification of nullability over a generic decoder.
-}
data NullableOrNot decoder a where
  NonNullable :: decoder a -> NullableOrNot decoder a
  Nullable :: decoder a -> NullableOrNot decoder (Maybe a)

{-|
Specify that a decoder produces a non-nullable value.
-}
nonNullable :: decoder a -> NullableOrNot decoder a
nonNullable = NonNullable

{-|
Specify that a decoder produces a nullable value.
-}
nullable :: decoder a -> NullableOrNot decoder (Maybe a)
nullable = Nullable


-- * Value
-------------------------

{-|
Decoder of a value.
-}
newtype Value a = Value (Value.Value a)
  deriving (Functor)

type role Value representational

{-|
Decoder of the @BOOL@ values.
-}
{-# INLINABLE bool #-}
bool :: Value Bool
bool = Value (Value.decoder (const A.bool))

{-|
Decoder of the @INT2@ values.
-}
{-# INLINABLE int2 #-}
int2 :: Value Int16
int2 = Value (Value.decoder (const A.int))

{-|
Decoder of the @INT4@ values.
-}
{-# INLINABLE int4 #-}
int4 :: Value Int32
int4 = Value (Value.decoder (const A.int))

{-|
Decoder of the @INT8@ values.
-}
{-# INLINABLE int8 #-}
int8 :: Value Int64
int8 = {-# SCC "int8" #-}
  Value (Value.decoder (const ({-# SCC "int8.int" #-} A.int)))

{-|
Decoder of the @FLOAT4@ values.
-}
{-# INLINABLE float4 #-}
float4 :: Value Float
float4 = Value (Value.decoder (const A.float4))

{-|
Decoder of the @FLOAT8@ values.
-}
{-# INLINABLE float8 #-}
float8 :: Value Double
float8 = Value (Value.decoder (const A.float8))

{-|
Decoder of the @NUMERIC@ values.
-}
{-# INLINABLE numeric #-}
numeric :: Value B.Scientific
numeric = Value (Value.decoder (const A.numeric))

{-|
Decoder of the @CHAR@ values.
Note that it supports Unicode values.
-}
{-# INLINABLE char #-}
char :: Value Char
char = Value (Value.decoder (const A.char))

{-|
Decoder of the @TEXT@ values.
-}
{-# INLINABLE text #-}
text :: Value Text
text = Value (Value.decoder (const A.text_strict))

{-|
Decoder of the @BYTEA@ values.
-}
{-# INLINABLE bytea #-}
bytea :: Value ByteString
bytea = Value (Value.decoder (const A.bytea_strict))

{-|
Decoder of the @DATE@ values.
-}
{-# INLINABLE date #-}
date :: Value B.Day
date = Value (Value.decoder (const A.date))

{-|
Decoder of the @TIMESTAMP@ values.
-}
{-# INLINABLE timestamp #-}
timestamp :: Value B.LocalTime
timestamp = Value (Value.decoder (Prelude.bool A.timestamp_float A.timestamp_int))

{-|
Decoder of the @TIMESTAMPTZ@ values.

/NOTICE/

Postgres does not store the timezone information of @TIMESTAMPTZ@.
Instead it stores a UTC value and performs silent conversions
to the currently set timezone, when dealt with in the text format.
However this library bypasses the silent conversions
and communicates with Postgres using the UTC values directly.
-}
{-# INLINABLE timestamptz #-}
timestamptz :: Value B.UTCTime
timestamptz = Value (Value.decoder (Prelude.bool A.timestamptz_float A.timestamptz_int))

{-|
Decoder of the @TIME@ values.
-}
{-# INLINABLE time #-}
time :: Value B.TimeOfDay
time = Value (Value.decoder (Prelude.bool A.time_float A.time_int))

{-|
Decoder of the @TIMETZ@ values.

Unlike in case of @TIMESTAMPTZ@,
Postgres does store the timezone information for @TIMETZ@.
However the Haskell's \"time\" library does not contain any composite type,
that fits the task, so we use a pair of 'TimeOfDay' and 'TimeZone'
to represent a value on the Haskell's side.
-}
{-# INLINABLE timetz #-}
timetz :: Value (B.TimeOfDay, B.TimeZone)
timetz = Value (Value.decoder (Prelude.bool A.timetz_float A.timetz_int))

{-|
Decoder of the @INTERVAL@ values.
-}
{-# INLINABLE interval #-}
interval :: Value B.DiffTime
interval = Value (Value.decoder (Prelude.bool A.interval_float A.interval_int))

{-|
Decoder of the @UUID@ values.
-}
{-# INLINABLE uuid #-}
uuid :: Value B.UUID
uuid = Value (Value.decoder (const A.uuid))

{-|
Decoder of the @INET@ values.
-}
{-# INLINABLE inet #-}
inet :: Value (B.NetAddr B.IP)
inet = Value (Value.decoder (const A.inet))

{-|
Decoder of the @JSON@ values into a JSON AST.
-}
{-# INLINABLE json #-}
json :: Value B.Value
json = Value (Value.decoder (const A.json_ast))

{-|
Decoder of the @JSON@ values into a raw JSON 'ByteString'.
-}
{-# INLINABLE jsonBytes #-}
jsonBytes :: (ByteString -> Either Text a) -> Value a
jsonBytes fn = Value (Value.decoder (const (A.json_bytes fn)))

{-|
Decoder of the @JSONB@ values into a JSON AST.
-}
{-# INLINABLE jsonb #-}
jsonb :: Value B.Value
jsonb = Value (Value.decoder (const A.jsonb_ast))

{-|
Decoder of the @JSONB@ values into a raw JSON 'ByteString'.
-}
{-# INLINABLE jsonbBytes #-}
jsonbBytes :: (ByteString -> Either Text a) -> Value a
jsonbBytes fn = Value (Value.decoder (const (A.jsonb_bytes fn)))

{-|
Lift a custom value decoder function to a 'Value' decoder.
-}
{-# INLINABLE custom #-}
custom :: (Bool -> ByteString -> Either Text a) -> Value a
custom fn = Value (Value.decoderFn fn)

{-|
Refine a value decoder, lifting the possible error to the session level.
-}
{-# INLINABLE refine #-}
refine :: (a -> Either Text b) -> Value a -> Value b
refine fn (Value v) = Value (Value.Value (\b -> A.refine fn (Value.run v b)))

{-|
A generic decoder of @HSTORE@ values.

Here's how you can use it to construct a specific value:

@
x :: Value [(Text, Maybe Text)]
x = hstore 'replicateM'
@
-}
{-# INLINABLE hstore #-}
hstore :: (forall m. Monad m => Int -> m (Text, Maybe Text) -> m a) -> Value a
hstore replicateM = Value (Value.decoder (const (A.hstore replicateM A.text_strict A.text_strict)))

{-|
Given a partial mapping from text to value,
produces a decoder of that value.
-}
enum :: (Text -> Maybe a) -> Value a
enum mapping = Value (Value.decoder (const (A.enum mapping)))

{-|
Lift an 'Array' decoder to a 'Value' decoder.
-}
{-# INLINABLE array #-}
array :: Array a -> Value a
array (Array imp) = Value (Value.decoder (Array.run imp))

{-|
Lift a value decoder of element into a unidimensional array decoder producing a list.

This function is merely a shortcut to the following expression:

@
('array' . 'dimension' Control.Monad.'replicateM' . 'element')
@

Please notice that in case of multidimensional arrays nesting 'listArray' decoder
won't work. You have to explicitly construct the array decoder using 'array'.
-}
{-# INLINE listArray #-}
listArray :: NullableOrNot Value element -> Value [element]
listArray = array . dimension replicateM . element

{-|
Lift a value decoder of element into a unidimensional array decoder producing a generic vector.

This function is merely a shortcut to the following expression:

@
('array' . 'dimension' Data.Vector.Generic.'GenericVector.replicateM' . 'element')
@

Please notice that in case of multidimensional arrays nesting 'vectorArray' decoder
won't work. You have to explicitly construct the array decoder using 'array'.
-}
{-# INLINE vectorArray #-}
vectorArray :: GenericVector.Vector vector element => NullableOrNot Value element -> Value (vector element)
vectorArray = array . dimension GenericVector.replicateM . element

{-|
Lift a 'Composite' decoder to a 'Value' decoder.
-}
{-# INLINABLE composite #-}
composite :: Composite a -> Value a
composite (Composite imp) = Value (Value.decoder (Composite.run imp))


-- * Array decoders
-------------------------

{-|
A generic array decoder.

Here's how you can use it to produce a specific array value decoder:

@
x :: 'Value' [[Text]]
x = 'array' ('dimension' 'replicateM' ('dimension' 'replicateM' ('element' ('nonNullable' 'text'))))
@
-}
newtype Array a = Array (Array.Array a)
  deriving (Functor)

{-|
A function for parsing a dimension of an array.
Provides support for multi-dimensional arrays.

Accepts:

* An implementation of the @replicateM@ function
(@Control.Monad.'Control.Monad.replicateM'@, @Data.Vector.'Data.Vector.replicateM'@),
which determines the output value.

* A decoder of its components, which can be either another 'dimension' or 'element'.
-}
{-# INLINABLE dimension #-}
dimension :: (forall m. Monad m => Int -> m a -> m b) -> Array a -> Array b
dimension replicateM (Array imp) = Array (Array.dimension replicateM imp)

{-|
Lift a 'Value' decoder into an 'Array' decoder for parsing of leaf values.
-}
{-# INLINABLE element #-}
element :: NullableOrNot Value a -> Array a
element = \ case
  NonNullable (Value imp) -> Array (Array.nonNullValue (Value.run imp))
  Nullable (Value imp) -> Array (Array.value (Value.run imp))


-- * Composite decoders
-------------------------

{-|
Composable decoder of composite values (rows, records).
-}
newtype Composite a = Composite (Composite.Composite a)
  deriving (Functor, Applicative, Monad, MonadFail)

{-|
Lift a 'Value' decoder into a 'Composite' decoder for parsing of component values.
-}
field :: NullableOrNot Value a -> Composite a
field = \ case
  NonNullable (Value imp) -> Composite (Composite.nonNullValue (Value.run imp))
  Nullable (Value imp) -> Composite (Composite.value (Value.run imp))
