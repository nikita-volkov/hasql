-- |
-- A DSL for declaration of query parameter encoders.
module Hasql.Encoders
(
  -- * Params
  Params,
  unit,
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
  enum,
  unknown,
  -- * Array
  Array,
  arrayValue,
  arrayNullableValue,
  arrayDimension,
)
where

import Hasql.Prelude hiding (bool)
import qualified PostgreSQL.Binary.Encoder as Encoder
import qualified Data.Aeson as Aeson
import qualified Hasql.Encoders.Params as Params
import qualified Hasql.Encoders.Value as Value
import qualified Hasql.Encoders.Array as Array
import qualified Hasql.PTI as PTI
import qualified Hasql.Prelude as Prelude


-- * Parameters Product Encoder
-------------------------

-- |
-- Encoder of some representation of the parameters product.
-- 
-- Has instances of 'Contravariant', 'Divisible' and 'Monoid',
-- which you can use to compose multiple parameters together.
-- E.g.,
-- 
-- @
-- someParamsEncoder :: 'Params' (Int64, Maybe Text)
-- someParamsEncoder =
--   'contramap' 'fst' ('value' 'int8') '<>'
--   'contramap' 'snd' ('nullableValue' 'text')
-- @
-- 
-- As a general solution for tuples of any arity, instead of 'fst' and 'snd',
-- consider the functions of the @contrazip@ family
-- from the \"contravariant-extras\" package.
-- E.g., here's how you can achieve the same as the above:
-- 
-- @
-- someParamsEncoder :: 'Params' (Int64, Maybe Text)
-- someParamsEncoder =
--   'contrazip2' ('value' 'int8') ('nullableValue' 'text')
-- @
-- 
-- Here's how you can implement encoders for custom composite types:
-- 
-- @
-- data Person =
--   Person { name :: Text, gender :: Gender, age :: Int }
-- 
-- data Gender =
--   Male | Female
-- 
-- personParams :: 'Params' Person
-- personParams =
--   'contramap' name ('value' 'text') '<>'
--   'contramap' gender ('value' genderValue) '<>'
--   'contramap' (fromIntegral . age) ('value' 'int8')
-- 
-- genderValue :: 'Value' Gender
-- genderValue =
--   'contramap' genderText 'text'
--   where
--     genderText gender =
--       case gender of
--         Male -> "male"
--         Female -> "female"
-- @
-- 
newtype Params a =
  Params (Params.Params a)
  deriving (Contravariant, Divisible, Monoid)

-- |
-- Encode no parameters.
-- 
{-# INLINABLE unit #-}
unit :: Params ()
unit =
  Params mempty

-- |
-- Lift an individual value encoder to a parameters encoder.
-- 
{-# INLINABLE value #-}
value :: Value a -> Params a
value (Value x) =
  Params (Params.value x)

-- |
-- Lift an individual nullable value encoder to a parameters encoder.
-- 
{-# INLINABLE nullableValue #-}
nullableValue :: Value a -> Params (Maybe a)
nullableValue (Value x) =
  Params (Params.nullableValue x)


-- ** Instances
-------------------------

-- |
-- Maps to 'unit'.
instance Default (Params ()) where
  {-# INLINE def #-}
  def =
    unit

instance Default (Value a) => Default (Params (Identity a)) where
  {-# INLINE def #-}
  def =
    contramap runIdentity (value def)

instance (Default (Value a1), Default (Value a2)) => Default (Params (a1, a2)) where
  {-# INLINE def #-}
  def =
    contrazip2 (value def) (value def)

instance (Default (Value a1), Default (Value a2), Default (Value a3)) => Default (Params (a1, a2, a3)) where
  {-# INLINE def #-}
  def =
    contrazip3 (value def) (value def) (value def)

instance (Default (Value a1), Default (Value a2), Default (Value a3), Default (Value a4)) => Default (Params (a1, a2, a3, a4)) where
  {-# INLINE def #-}
  def =
    contrazip4 (value def) (value def) (value def) (value def)

instance (Default (Value a1), Default (Value a2), Default (Value a3), Default (Value a4), Default (Value a5)) => Default (Params (a1, a2, a3, a4, a5)) where
  {-# INLINE def #-}
  def =
    contrazip5 (value def) (value def) (value def) (value def) (value def)


-- * Value Encoder
-------------------------

-- |
-- An individual value encoder.
-- Will be mapped to a single placeholder in the query.
-- 
newtype Value a =
  Value (Value.Value a)
  deriving (Contravariant)

-- |
-- Encoder of @BOOL@ values.
{-# INLINABLE bool #-}
bool :: Value Bool
bool =
  Value (Value.unsafePTI PTI.bool (const Encoder.bool))

-- |
-- Encoder of @INT2@ values.
{-# INLINABLE int2 #-}
int2 :: Value Int16
int2 =
  Value (Value.unsafePTI PTI.int2 (const Encoder.int2_int16))

-- |
-- Encoder of @INT4@ values.
{-# INLINABLE int4 #-}
int4 :: Value Int32
int4 =
  Value (Value.unsafePTI PTI.int4 (const Encoder.int4_int32))

-- |
-- Encoder of @INT8@ values.
{-# INLINABLE int8 #-}
int8 :: Value Int64
int8 =
  Value (Value.unsafePTI PTI.int8 (const Encoder.int8_int64))

-- |
-- Encoder of @FLOAT4@ values.
{-# INLINABLE float4 #-}
float4 :: Value Float
float4 =
  Value (Value.unsafePTI PTI.float4 (const Encoder.float4))

-- |
-- Encoder of @FLOAT8@ values.
{-# INLINABLE float8 #-}
float8 :: Value Double
float8 =
  Value (Value.unsafePTI PTI.float8 (const Encoder.float8))

-- |
-- Encoder of @NUMERIC@ values.
{-# INLINABLE numeric #-}
numeric :: Value Scientific
numeric =
  Value (Value.unsafePTI PTI.numeric (const Encoder.numeric))

-- |
-- Encoder of @CHAR@ values.
-- Note that it supports UTF-8 values and
-- identifies itself under the @TEXT@ OID because of that.
{-# INLINABLE char #-}
char :: Value Char
char =
  Value (Value.unsafePTI PTI.text (const Encoder.char))

-- |
-- Encoder of @TEXT@ values.
{-# INLINABLE text #-}
text :: Value Text
text =
  Value (Value.unsafePTI PTI.text (const Encoder.text_strict))

-- |
-- Encoder of @BYTEA@ values.
{-# INLINABLE bytea #-}
bytea :: Value ByteString
bytea =
  Value (Value.unsafePTI PTI.bytea (const Encoder.bytea_strict))

-- |
-- Encoder of @DATE@ values.
{-# INLINABLE date #-}
date :: Value Day
date =
  Value (Value.unsafePTI PTI.date (const Encoder.date))

-- |
-- Encoder of @TIMESTAMP@ values.
{-# INLINABLE timestamp #-}
timestamp :: Value LocalTime
timestamp =
  Value (Value.unsafePTI PTI.timestamp (Prelude.bool Encoder.timestamp_int Encoder.timestamp_float))

-- |
-- Encoder of @TIMESTAMPTZ@ values.
{-# INLINABLE timestamptz #-}
timestamptz :: Value UTCTime
timestamptz =
  Value (Value.unsafePTI PTI.timestamptz (Prelude.bool Encoder.timestamptz_int Encoder.timestamptz_float))

-- |
-- Encoder of @TIME@ values.
{-# INLINABLE time #-}
time :: Value TimeOfDay
time =
  Value (Value.unsafePTI PTI.time (Prelude.bool Encoder.time_int Encoder.time_float))

-- |
-- Encoder of @TIMETZ@ values.
{-# INLINABLE timetz #-}
timetz :: Value (TimeOfDay, TimeZone)
timetz =
  Value (Value.unsafePTI PTI.timetz (Prelude.bool Encoder.timetz_int Encoder.timetz_float))

-- |
-- Encoder of @INTERVAL@ values.
{-# INLINABLE interval #-}
interval :: Value DiffTime
interval =
  Value (Value.unsafePTI PTI.interval (Prelude.bool Encoder.interval_int Encoder.interval_float))

-- |
-- Encoder of @UUID@ values.
{-# INLINABLE uuid #-}
uuid :: Value UUID
uuid =
  Value (Value.unsafePTI PTI.uuid (const Encoder.uuid))

-- |
-- Encoder of @JSON@ values.
{-# INLINABLE json #-}
json :: Value Aeson.Value
json =
  Value (Value.unsafePTI PTI.json (const Encoder.json))

-- |
-- Unlifts the 'Array' encoder to the plain 'Value' encoder.
{-# INLINABLE array #-}
array :: Array a -> Value a
array (Array imp) =
  Array.run imp & \(arrayOID, encoder') ->
    Value (Value.Value arrayOID arrayOID encoder')

-- |
-- Given a function,
-- which maps the value into the textual enum label from the DB side,
-- produces a encoder of that value.
{-# INLINABLE enum #-}
enum :: (a -> Text) -> Value a
enum mapping =
  Value (Value.unsafePTI PTI.text (const (Encoder.enum mapping)))

-- |
-- Identifies the value with the PostgreSQL's \"unknown\" type,
-- thus leaving it up to Postgres to infer the actual type of the value.
-- 
-- The bytestring needs to be encoded according to the Postgres\' binary format
-- of the type it expects.
-- 
-- Essentially this is a low-level hook for encoding of values with custom codecs.
-- The
-- <http://hackage.haskell.org/package/postgresql-binary "postgresql-binary">
-- library will provide you with the toolchain.
-- 
{-# INLINABLE unknown #-}
unknown :: Value ByteString
unknown =
  Value (Value.unsafePTI PTI.unknown (const Encoder.bytea_strict))


-- ** Instances
-------------------------

-- | Maps to 'bool'.
instance Default (Value Bool) where
  {-# INLINE def #-}
  def =
    bool

-- | Maps to 'int2'.
instance Default (Value Int16) where
  {-# INLINE def #-}
  def =
    int2

-- | Maps to 'int4'.
instance Default (Value Int32) where
  {-# INLINE def #-}
  def =
    int4

-- | Maps to 'int8'.
instance Default (Value Int64) where
  {-# INLINE def #-}
  def =
    int8

-- | Maps to 'float4'.
instance Default (Value Float) where
  {-# INLINE def #-}
  def =
    float4

-- | Maps to 'float8'.
instance Default (Value Double) where
  {-# INLINE def #-}
  def =
    float8

-- | Maps to 'numeric'.
instance Default (Value Scientific) where
  {-# INLINE def #-}
  def =
    numeric

-- | Maps to 'char'.
instance Default (Value Char) where
  {-# INLINE def #-}
  def =
    char

-- | Maps to 'text'.
instance Default (Value Text) where
  {-# INLINE def #-}
  def =
    text

-- | Maps to 'bytea'.
instance Default (Value ByteString) where
  {-# INLINE def #-}
  def =
    bytea

-- | Maps to 'date'.
instance Default (Value Day) where
  {-# INLINE def #-}
  def =
    date

-- | Maps to 'timestamp'.
instance Default (Value LocalTime) where
  {-# INLINE def #-}
  def =
    timestamp

-- | Maps to 'timestamptz'.
instance Default (Value UTCTime) where
  {-# INLINE def #-}
  def =
    timestamptz

-- | Maps to 'time'.
instance Default (Value TimeOfDay) where
  {-# INLINE def #-}
  def =
    time

-- | Maps to 'timetz'.
instance Default (Value (TimeOfDay, TimeZone)) where
  {-# INLINE def #-}
  def =
    timetz

-- | Maps to 'interval'.
instance Default (Value DiffTime) where
  {-# INLINE def #-}
  def =
    interval

-- | Maps to 'uuid'.
instance Default (Value UUID) where
  {-# INLINE def #-}
  def =
    uuid

-- | Maps to 'json'.
instance Default (Value Aeson.Value) where
  {-# INLINE def #-}
  def =
    json


-- * Array
-------------------------

-- |
-- A generic array encoder.
-- 
-- Here's an example of its usage:
-- 
-- >x :: Value [[Int64]]
-- >x =
-- >  array (arrayDimension foldl' (arrayDimension foldl' (arrayValue int8)))
-- 
newtype Array a =
  Array (Array.Array a)

-- |
-- Lifts the 'Value' encoder into the 'Array' encoder of a non-nullable value.
{-# INLINABLE arrayValue #-}
arrayValue :: Value a -> Array a
arrayValue (Value (Value.Value elementOID arrayOID encoder')) =
  Array (Array.value elementOID arrayOID encoder')

-- |
-- Lifts the 'Value' encoder into the 'Array' encoder of a nullable value.
{-# INLINABLE arrayNullableValue #-}
arrayNullableValue :: Value a -> Array (Maybe a)
arrayNullableValue (Value (Value.Value elementOID arrayOID encoder')) =
  Array (Array.nullableValue elementOID arrayOID encoder')

-- |
-- An encoder of an array dimension,
-- which thus provides support for multidimensional arrays.
-- 
-- Accepts:
-- 
-- * An implementation of the left-fold operation,
-- such as @Data.Foldable.'foldl''@,
-- which determines the input value.
-- 
-- * A component encoder, which can be either another 'arrayDimension',
-- 'arrayValue' or 'arrayNullableValue'.
-- 
{-# INLINABLE arrayDimension #-}
arrayDimension :: (forall a. (a -> b -> a) -> a -> c -> a) -> Array b -> Array c
arrayDimension foldl (Array imp) =
  Array (Array.dimension foldl imp)

