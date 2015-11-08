module Hasql.Serialization
(
  -- * Params
  Params,
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
import qualified Hasql.Serialization.Params as Params
import qualified Hasql.Serialization.Value as Value
import qualified Hasql.Serialization.Array as Array
import qualified Hasql.PTI as PTI
import qualified Hasql.Prelude as Prelude


-- * Parameters Product Serializer
-------------------------

-- |
-- Serializer of some representation of a parameters product.
-- 
-- Has instances of 'Contravariant', 'Divisible' and 'Monoid',
-- which you can use to compose multiple parameters together.
-- E.g.,
-- 
-- >someParamsSerializer :: Params (Int64, Maybe Text)
-- >someParamsSerializer =
-- >  contramap fst (value int8) <>
-- >  contramap snd (nullableValue text)
-- 
newtype Params a =
  Params (Params.Params a)
  deriving (Contravariant, Divisible, Monoid)

-- |
-- Lift an individual value serializer to a parameters serializer.
-- 
{-# INLINABLE value #-}
value :: Value a -> Params a
value (Value x) =
  Params (Params.value x)

-- |
-- Lift an individual nullable value serializer to a parameters serializer.
-- 
{-# INLINABLE nullableValue #-}
nullableValue :: Value a -> Params (Maybe a)
nullableValue (Value x) =
  Params (Params.nullableValue x)


-- ** Instances
-------------------------

instance Default (Value a) => Default (Params (Identity a)) where
  {-# INLINE def #-}
  def =
    contramap runIdentity (value def)

instance (Default (Value a1), Default (Value a2)) => Default (Params (a1, a2)) where
  {-# INLINE def #-}
  def =
    contramap fst (value def) <>
    contramap snd (value def)


-- * Value Serializer
-------------------------

-- |
-- An individual value serializer.
-- Will be mapped to a single placeholder in the query.
-- 
newtype Value a =
  Value (Value.Value a)
  deriving (Contravariant)

{-# INLINABLE bool #-}
bool :: Value Bool
bool =
  Value (Value.unsafePTI PTI.bool (const Encoder.bool))

{-# INLINABLE int2 #-}
int2 :: Value Int16
int2 =
  Value (Value.unsafePTI PTI.int2 (const Encoder.int2_int16))

{-# INLINABLE int4 #-}
int4 :: Value Int32
int4 =
  Value (Value.unsafePTI PTI.int4 (const Encoder.int4_int32))

{-# INLINABLE int8 #-}
int8 :: Value Int64
int8 =
  Value (Value.unsafePTI PTI.int8 (const Encoder.int8_int64))

{-# INLINABLE float4 #-}
float4 :: Value Float
float4 =
  Value (Value.unsafePTI PTI.float4 (const Encoder.float4))

{-# INLINABLE float8 #-}
float8 :: Value Double
float8 =
  Value (Value.unsafePTI PTI.float8 (const Encoder.float8))

{-# INLINABLE numeric #-}
numeric :: Value Scientific
numeric =
  Value (Value.unsafePTI PTI.numeric (const Encoder.numeric))

{-# INLINABLE char #-}
char :: Value Char
char =
  Value (Value.unsafePTI PTI.char (const Encoder.char))

{-# INLINABLE text #-}
text :: Value Text
text =
  Value (Value.unsafePTI PTI.text (const Encoder.text_strict))

{-# INLINABLE bytea #-}
bytea :: Value ByteString
bytea =
  Value (Value.unsafePTI PTI.bytea (const Encoder.bytea_strict))

{-# INLINABLE date #-}
date :: Value Day
date =
  Value (Value.unsafePTI PTI.date (const Encoder.date))

{-# INLINABLE timestamp #-}
timestamp :: Value LocalTime
timestamp =
  Value (Value.unsafePTI PTI.timestamp (Prelude.bool Encoder.timestamp_int Encoder.timestamp_float))

{-# INLINABLE timestamptz #-}
timestamptz :: Value UTCTime
timestamptz =
  Value (Value.unsafePTI PTI.timestamptz (Prelude.bool Encoder.timestamptz_int Encoder.timestamptz_float))

{-# INLINABLE time #-}
time :: Value TimeOfDay
time =
  Value (Value.unsafePTI PTI.time (Prelude.bool Encoder.time_int Encoder.time_float))

{-# INLINABLE timetz #-}
timetz :: Value (TimeOfDay, TimeZone)
timetz =
  Value (Value.unsafePTI PTI.timetz (Prelude.bool Encoder.timetz_int Encoder.timetz_float))

{-# INLINABLE interval #-}
interval :: Value DiffTime
interval =
  Value (Value.unsafePTI PTI.interval (Prelude.bool Encoder.interval_int Encoder.interval_float))

{-# INLINABLE uuid #-}
uuid :: Value UUID
uuid =
  Value (Value.unsafePTI PTI.uuid (const Encoder.uuid))

{-# INLINABLE json #-}
json :: Value Aeson.Value
json =
  Value (Value.unsafePTI PTI.json (const Encoder.json))

{-# INLINABLE array #-}
array :: Array a -> Value a
array (Array imp) =
  Array.run imp & \(arrayOID, encoder') ->
    Value (Value.Value arrayOID arrayOID encoder')


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

newtype Array a =
  Array (Array.Array a)

{-# INLINABLE arrayValue #-}
arrayValue :: Value a -> Array a
arrayValue (Value (Value.Value elementOID arrayOID encoder')) =
  Array (Array.value elementOID arrayOID encoder')

{-# INLINABLE arrayNullableValue #-}
arrayNullableValue :: Value a -> Array (Maybe a)
arrayNullableValue (Value (Value.Value elementOID arrayOID encoder')) =
  Array (Array.nullableValue elementOID arrayOID encoder')

{-# INLINABLE arrayDimension #-}
arrayDimension :: (forall a. (a -> b -> a) -> a -> c -> a) -> Array b -> Array c
arrayDimension foldl (Array imp) =
  Array (Array.dimension foldl imp)

