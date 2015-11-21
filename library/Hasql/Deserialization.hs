-- |
-- A DSL for creating result deserializers.
module Hasql.Deserialization
(
  -- * Result
  Result,
  noResult,
  rowsAffected,
  singleRow,
  -- ** Specialized multi-row results
  maybeRow,
  rowsVector,
  rowsList,
  -- ** Multi-row traversers
  foldlRows,
  foldrRows,
  -- * Row
  Row,
  value,
  nullableValue,
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
  json,
  array,
  composite,
  hstore,
  enum,
  -- * Array
  Array,
  arrayDimension,
  arrayValue,
  arrayNullableValue,
  -- * Composite
  Composite,
  compositeValue,
  compositeNullableValue,
)
where

import Hasql.Prelude hiding (maybe, bool)
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified PostgreSQL.Binary.Decoder as Decoder
import qualified Hasql.Deserialization.Results as Results
import qualified Hasql.Deserialization.Result as Result
import qualified Hasql.Deserialization.Row as Row
import qualified Hasql.Deserialization.Value as Value
import qualified Hasql.Deserialization.Array as Array
import qualified Hasql.Deserialization.Composite as Composite
import qualified Hasql.Prelude as Prelude


-- * Result
-------------------------

-- |
-- Deserializer of a query result.
-- 
newtype Result a =
  Result (Results.Results a)
  deriving (Functor)

-- |
-- Deserialize no value from the result.
-- 
-- Useful for statements like @INSERT@ or @CREATE@.
-- 
{-# INLINABLE noResult #-}
noResult :: Result ()
noResult =
  Result (Results.single Result.unit)

-- |
-- Get the amount of rows affected by such statements as
-- @UPDATE@ or @DELETE@.
-- 
{-# INLINABLE rowsAffected #-}
rowsAffected :: Result Int64
rowsAffected =
  Result (Results.single Result.rowsAffected)

-- |
-- Exactly one row.
-- Will raise the 'Hasql.UnexpectedAmountOfRows' error if it's any other.
-- 
{-# INLINABLE singleRow #-}
singleRow :: Row a -> Result a
singleRow (Row row) =
  Result (Results.single (Result.single row))

-- ** Multi-row traversers
-------------------------

-- |
-- Foldl multiple rows.
-- 
{-# INLINABLE foldlRows #-}
foldlRows :: (a -> b -> a) -> a -> Row b -> Result a
foldlRows step init (Row row) =
  Result (Results.single (Result.foldl step init row))

-- |
-- Foldr multiple rows.
-- 
{-# INLINABLE foldrRows #-}
foldrRows :: (b -> a -> a) -> a -> Row b -> Result a
foldrRows step init (Row row) =
  Result (Results.single (Result.foldr step init row))

-- ** Specialized multi-row results
-------------------------

-- |
-- Maybe one row or none.
-- 
{-# INLINABLE maybeRow #-}
maybeRow :: Row a -> Result (Maybe a)
maybeRow (Row row) =
  Result (Results.single (Result.maybe row))

-- |
-- Zero or more rows packed into the vector.
-- 
-- It's recommended to prefer this function to 'rowsList',
-- since it performs notably faster.
-- 
{-# INLINABLE rowsVector #-}
rowsVector :: Row a -> Result (Vector a)
rowsVector (Row row) =
  Result (Results.single (Result.vector row))

-- |
-- Zero or more rows packed into the list.
-- 
{-# INLINABLE rowsList #-}
rowsList :: Row a -> Result [a]
rowsList =
  foldrRows strictCons []


-- ** Instances
-------------------------

-- | Maps to 'noResult'.
instance Default (Result ()) where
  {-# INLINE def #-}
  def =
    noResult

-- | Maps to 'rowsAffected'.
instance Default (Result Int64) where
  {-# INLINE def #-}
  def =
    rowsAffected

-- | Maps to @('maybeRow' def)@.
instance Default (Row a) => Default (Result (Maybe a)) where
  {-# INLINE def #-}
  def =
    maybeRow def

-- | Maps to @('rowsVector' def)@.
instance Default (Row a) => Default (Result (Vector a)) where
  {-# INLINE def #-}
  def =
    rowsVector def

-- | Maps to @('rowsList' def)@.
instance Default (Row a) => Default (Result ([] a)) where
  {-# INLINE def #-}
  def =
    rowsList def

-- | Maps to @(fmap Identity ('singleRow' def)@.
instance Default (Row a) => Default (Result (Identity a)) where
  {-# INLINE def #-}
  def =
    fmap Identity (singleRow def)


-- * Row
-------------------------

-- |
-- Deserializer of an individual row,
-- which gets composed of column value deserializers.
-- 
-- E.g.:
-- 
-- >x :: Row (Maybe Int64, Text, TimeOfDay)
-- >x =
-- >  (,,) <$> nullableValue int8 <*> value text <*> value time
-- 
newtype Row a =
  Row (Row.Row a)
  deriving (Functor, Applicative, Monad)

-- |
-- Lift an individual non-nullable value deserializer to a composable row deserializer.
-- 
{-# INLINABLE value #-}
value :: Value a -> Row a
value (Value imp) =
  Row (Row.nonNullValue imp)

-- |
-- Lift an individual nullable value deserializer to a composable row deserializer.
-- 
{-# INLINABLE nullableValue #-}
nullableValue :: Value a -> Row (Maybe a)
nullableValue (Value imp) =
  Row (Row.value imp)


-- ** Instances
-------------------------

instance Default (Value a) => Default (Row (Identity a)) where
  {-# INLINE def #-}
  def =
    fmap Identity (value def)

instance Default (Value a) => Default (Row (Maybe a)) where
  {-# INLINE def #-}
  def =
    nullableValue def

instance (Default (Value a1), Default (Value a2)) => Default (Row (a1, a2)) where
  {-# INLINE def #-}
  def =
    ap (fmap (,) (value def)) (value def)


-- * Value
-------------------------

-- |
-- Deserializer of an individual value.
-- 
newtype Value a =
  Value (Value.Value a)
  deriving (Functor)


-- ** Plain value deserializers
-------------------------

-- |
-- Deserializer of the @BOOL@ values.
-- 
{-# INLINABLE bool #-}
bool :: Value Bool
bool =
  Value (Value.decoder (const Decoder.bool))

-- |
-- Deserializer of the @INT2@ values.
-- 
{-# INLINABLE int2 #-}
int2 :: Value Int16
int2 =
  Value (Value.decoder (const Decoder.int))

-- |
-- Deserializer of the @INT4@ values.
-- 
{-# INLINABLE int4 #-}
int4 :: Value Int32
int4 =
  Value (Value.decoder (const Decoder.int))

-- |
-- Deserializer of the @INT8@ values.
-- 
{-# INLINABLE int8 #-}
int8 :: Value Int64
int8 =
  {-# SCC "int8" #-} 
  Value (Value.decoder (const ({-# SCC "int8.int" #-} Decoder.int)))

-- |
-- Deserializer of the @FLOAT4@ values.
-- 
{-# INLINABLE float4 #-}
float4 :: Value Float
float4 =
  Value (Value.decoder (const Decoder.float4))

-- |
-- Deserializer of the @FLOAT8@ values.
-- 
{-# INLINABLE float8 #-}
float8 :: Value Double
float8 =
  Value (Value.decoder (const Decoder.float8))

-- |
-- Deserializer of the @NUMERIC@ values.
-- 
{-# INLINABLE numeric #-}
numeric :: Value Scientific
numeric =
  Value (Value.decoder (const Decoder.numeric))

-- |
-- Deserializer of the @CHAR@ values.
-- Note that it supports UTF-8 values.
{-# INLINABLE char #-}
char :: Value Char
char =
  Value (Value.decoder (const Decoder.char))

-- |
-- Deserializer of the @TEXT@ values.
-- 
{-# INLINABLE text #-}
text :: Value Text
text =
  Value (Value.decoder (const Decoder.text_strict))

-- |
-- Deserializer of the @BYTEA@ values.
-- 
{-# INLINABLE bytea #-}
bytea :: Value ByteString
bytea =
  Value (Value.decoder (const Decoder.bytea_strict))

-- |
-- Deserializer of the @DATE@ values.
-- 
{-# INLINABLE date #-}
date :: Value Day
date =
  Value (Value.decoder (const Decoder.date))

-- |
-- Deserializer of the @TIMESTAMP@ values.
-- 
{-# INLINABLE timestamp #-}
timestamp :: Value LocalTime
timestamp =
  Value (Value.decoder (Prelude.bool Decoder.timestamp_float Decoder.timestamp_int))

-- |
-- Deserializer of the @TIMESTAMPTZ@ values.
-- 
-- /NOTICE/
-- 
-- Postgres does not store the timezone information of @TIMESTAMPTZ@.
-- Instead it stores a UTC value and performs silent conversions
-- to the currently set timezone, when dealt with in the text format.
-- However this library bypasses the silent conversions
-- and communicates with Postgres using the UTC values directly.
{-# INLINABLE timestamptz #-}
timestamptz :: Value UTCTime
timestamptz =
  Value (Value.decoder (Prelude.bool Decoder.timestamptz_float Decoder.timestamptz_int))

-- |
-- Deserializer of the @TIME@ values.
-- 
{-# INLINABLE time #-}
time :: Value TimeOfDay
time =
  Value (Value.decoder (Prelude.bool Decoder.time_float Decoder.time_int))

-- |
-- Deserializer of the @TIMETZ@ values.
-- 
-- Unlike in case of @TIMESTAMPTZ@, 
-- Postgres does store the timezone information for @TIMETZ@.
-- However the Haskell's \"time\" library does not contain any composite type,
-- that fits the task, so we use a pair of 'TimeOfDay' and 'TimeZone'
-- to represent a value on the Haskell's side.
{-# INLINABLE timetz #-}
timetz :: Value (TimeOfDay, TimeZone)
timetz =
  Value (Value.decoder (Prelude.bool Decoder.timetz_float Decoder.timetz_int))

-- |
-- Deserializer of the @INTERVAL@ values.
-- 
{-# INLINABLE interval #-}
interval :: Value DiffTime
interval =
  Value (Value.decoder (Prelude.bool Decoder.interval_float Decoder.interval_int))

-- |
-- Deserializer of the @UUID@ values.
-- 
{-# INLINABLE uuid #-}
uuid :: Value UUID
uuid =
  Value (Value.decoder (const Decoder.uuid))

-- |
-- Deserializer of the @JSON@ values.
-- 
{-# INLINABLE json #-}
json :: Value Aeson.Value
json =
  Value (Value.decoder (const Decoder.json))


-- ** Composite value deserializers
-------------------------

-- |
-- Lifts the 'Array' deserializer to the 'Value' deserializer.
-- 
{-# INLINABLE array #-}
array :: Array a -> Value a
array (Array imp) =
  Value (Value.decoder (Array.run imp))

-- |
-- Lifts the 'Composite' deserializer to the 'Value' deserializer.
-- 
{-# INLINABLE composite #-}
composite :: Composite a -> Value a
composite (Composite imp) =
  Value (Value.decoder (Composite.run imp))

-- |
-- A generic deserializer of @HSTORE@ values.
-- 
-- Here's how you can use it to construct a specific value:
-- 
-- @
-- x :: Value [(Text, Maybe Text)]
-- x =
--   hstore 'replicateM'
-- @
-- 
{-# INLINABLE hstore #-}
hstore :: (forall m. Monad m => Int -> m (Text, Maybe Text) -> m a) -> Value a
hstore replicateM =
  Value (Value.decoder (const (Decoder.hstore replicateM Decoder.text_strict Decoder.text_strict)))

-- |
-- Given a partial mapping from text to value,
-- produces a deserializer of that value.
enum :: (Text -> Maybe a) -> Value a
enum mapping =
  Value (Value.decoder (const (Decoder.enum mapping)))


-- ** Instances
-------------------------

-- |
-- Maps to 'bool'.
instance Default (Value Bool) where
  {-# INLINE def #-}
  def =
    bool

-- |
-- Maps to 'int2'.
instance Default (Value Int16) where
  {-# INLINE def #-}
  def =
    int2

-- |
-- Maps to 'int4'.
instance Default (Value Int32) where
  {-# INLINE def #-}
  def =
    int4

-- |
-- Maps to 'int8'.
instance Default (Value Int64) where
  {-# INLINE def #-}
  def =
    int8

-- |
-- Maps to 'float4'.
instance Default (Value Float) where
  {-# INLINE def #-}
  def =
    float4

-- |
-- Maps to 'float8'.
instance Default (Value Double) where
  {-# INLINE def #-}
  def =
    float8

-- |
-- Maps to 'numeric'.
instance Default (Value Scientific) where
  {-# INLINE def #-}
  def =
    numeric

-- |
-- Maps to 'char'.
instance Default (Value Char) where
  {-# INLINE def #-}
  def =
    char

-- |
-- Maps to 'text'.
instance Default (Value Text) where
  {-# INLINE def #-}
  def =
    text

-- |
-- Maps to 'bytea'.
instance Default (Value ByteString) where
  {-# INLINE def #-}
  def =
    bytea

-- |
-- Maps to 'date'.
instance Default (Value Day) where
  {-# INLINE def #-}
  def =
    date

-- |
-- Maps to 'timestamp'.
instance Default (Value LocalTime) where
  {-# INLINE def #-}
  def =
    timestamp

-- |
-- Maps to 'timestamptz'.
instance Default (Value UTCTime) where
  {-# INLINE def #-}
  def =
    timestamptz

-- |
-- Maps to 'time'.
instance Default (Value TimeOfDay) where
  {-# INLINE def #-}
  def =
    time

-- |
-- Maps to 'timetz'.
instance Default (Value (TimeOfDay, TimeZone)) where
  {-# INLINE def #-}
  def =
    timetz

-- |
-- Maps to 'interval'.
instance Default (Value DiffTime) where
  {-# INLINE def #-}
  def =
    interval

-- |
-- Maps to 'uuid'.
instance Default (Value UUID) where
  {-# INLINE def #-}
  def =
    uuid

-- |
-- Maps to 'json'.
instance Default (Value Aeson.Value) where
  {-# INLINE def #-}
  def =
    json


-- * Array deserializers
-------------------------

-- |
-- A generic array deserializer.
-- 
-- Here's how you can use it to produce a specific array value deserializer:
-- 
-- @
-- x :: Value [[Text]]
-- x =
--   array (arrayDimension 'replicateM' (arrayDimension 'replicateM' (arrayValue text)))
-- @
-- 
newtype Array a =
  Array (Array.Array a)
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
-- * A deserializer of its components, which can be either another 'arrayDimension',
-- 'arrayValue' or 'arrayNullableValue'.
-- 
{-# INLINABLE arrayDimension #-}
arrayDimension :: (forall m. Monad m => Int -> m a -> m b) -> Array a -> Array b
arrayDimension replicateM (Array imp) =
  Array (Array.dimension replicateM imp)

-- |
-- Lift a 'Value' deserializer into an 'Array' deserializer for parsing of non-nullable leaf values.
{-# INLINABLE arrayValue #-}
arrayValue :: Value a -> Array a
arrayValue (Value imp) =
  Array (Array.nonNullValue (Value.run imp))

-- |
-- Lift a 'Value' deserializer into an 'Array' deserializer for parsing of nullable leaf values.
{-# INLINABLE arrayNullableValue #-}
arrayNullableValue :: Value a -> Array (Maybe a)
arrayNullableValue (Value imp) =
  Array (Array.value (Value.run imp))


-- * Composite deserializers
-------------------------

-- |
-- Composable deserializer of composite values (rows, records).
newtype Composite a =
  Composite (Composite.Composite a)
  deriving (Functor, Applicative, Monad)

-- |
-- Lift a 'Value' deserializer into an 'Composite' deserializer for parsing of non-nullable leaf values.
{-# INLINABLE compositeValue #-}
compositeValue :: Value a -> Composite a
compositeValue (Value imp) =
  Composite (Composite.nonNullValue (Value.run imp))

-- |
-- Lift a 'Value' deserializer into an 'Composite' deserializer for parsing of nullable leaf values.
{-# INLINABLE compositeNullableValue #-}
compositeNullableValue :: Value a -> Composite (Maybe a)
compositeNullableValue (Value imp) =
  Composite (Composite.value (Value.run imp))

