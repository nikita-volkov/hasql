-- |
-- A DSL for declaration of query parameter encoders.
module Hasql.Encoders
(
  -- * Params
  Params,
  unit,
  param,
  nullableParam,
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
  enum,
  unknown,
  -- * Array
  Array,
  element,
  nullableElement,
  dimension,
  -- ** Insert Many
  -- $insertMany
)
where

import Hasql.Private.Prelude hiding (bool)
import qualified PostgreSQL.Binary.Encoding as A
import qualified PostgreSQL.Binary.Data as B
import qualified Text.Builder as C
import qualified Hasql.Private.Encoders.Params as Params
import qualified Hasql.Private.Encoders.Value as Value
import qualified Hasql.Private.Encoders.Array as Array
import qualified Hasql.Private.PTI as PTI
import qualified Hasql.Private.Prelude as Prelude

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
--   'contramap' 'fst' ('param' 'int8') '<>'
--   'contramap' 'snd' ('nullableParam' 'text')
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
--   'contrazip2' ('param' 'int8') ('nullableParam' 'text')
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
--   'contramap' name ('param' 'text') '<>'
--   'contramap' gender ('param' genderValue) '<>'
--   'contramap' (fromIntegral . age) ('param' 'int8')
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
  deriving (Contravariant, Divisible, Decidable, Monoid, Semigroup)

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
{-# INLINABLE param #-}
param :: Value a -> Params a
param (Value x) =
  Params (Params.value x)

-- |
-- Lift an individual nullable value encoder to a parameters encoder.
-- 
{-# INLINABLE nullableParam #-}
nullableParam :: Value a -> Params (Maybe a)
nullableParam (Value x) =
  Params (Params.nullableValue x)


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
  Value (Value.unsafePTIWithShow PTI.bool (const A.bool))

-- |
-- Encoder of @INT2@ values.
{-# INLINABLE int2 #-}
int2 :: Value Int16
int2 =
  Value (Value.unsafePTIWithShow PTI.int2 (const A.int2_int16))

-- |
-- Encoder of @INT4@ values.
{-# INLINABLE int4 #-}
int4 :: Value Int32
int4 =
  Value (Value.unsafePTIWithShow PTI.int4 (const A.int4_int32))

-- |
-- Encoder of @INT8@ values.
{-# INLINABLE int8 #-}
int8 :: Value Int64
int8 =
  Value (Value.unsafePTIWithShow PTI.int8 (const A.int8_int64))

-- |
-- Encoder of @FLOAT4@ values.
{-# INLINABLE float4 #-}
float4 :: Value Float
float4 =
  Value (Value.unsafePTIWithShow PTI.float4 (const A.float4))

-- |
-- Encoder of @FLOAT8@ values.
{-# INLINABLE float8 #-}
float8 :: Value Double
float8 =
  Value (Value.unsafePTIWithShow PTI.float8 (const A.float8))

-- |
-- Encoder of @NUMERIC@ values.
{-# INLINABLE numeric #-}
numeric :: Value B.Scientific
numeric =
  Value (Value.unsafePTIWithShow PTI.numeric (const A.numeric))

-- |
-- Encoder of @CHAR@ values.
-- Note that it supports UTF-8 values and
-- identifies itself under the @TEXT@ OID because of that.
{-# INLINABLE char #-}
char :: Value Char
char =
  Value (Value.unsafePTIWithShow PTI.text (const A.char_utf8))

-- |
-- Encoder of @TEXT@ values.
{-# INLINABLE text #-}
text :: Value Text
text =
  Value (Value.unsafePTIWithShow PTI.text (const A.text_strict))

-- |
-- Encoder of @BYTEA@ values.
{-# INLINABLE bytea #-}
bytea :: Value ByteString
bytea =
  Value (Value.unsafePTIWithShow PTI.bytea (const A.bytea_strict))

-- |
-- Encoder of @DATE@ values.
{-# INLINABLE date #-}
date :: Value B.Day
date =
  Value (Value.unsafePTIWithShow PTI.date (const A.date))

-- |
-- Encoder of @TIMESTAMP@ values.
{-# INLINABLE timestamp #-}
timestamp :: Value B.LocalTime
timestamp =
  Value (Value.unsafePTIWithShow PTI.timestamp (Prelude.bool A.timestamp_float A.timestamp_int))

-- |
-- Encoder of @TIMESTAMPTZ@ values.
{-# INLINABLE timestamptz #-}
timestamptz :: Value B.UTCTime
timestamptz =
  Value (Value.unsafePTIWithShow PTI.timestamptz (Prelude.bool A.timestamptz_float A.timestamptz_int))

-- |
-- Encoder of @TIME@ values.
{-# INLINABLE time #-}
time :: Value B.TimeOfDay
time =
  Value (Value.unsafePTIWithShow PTI.time (Prelude.bool A.time_float A.time_int))

-- |
-- Encoder of @TIMETZ@ values.
{-# INLINABLE timetz #-}
timetz :: Value (B.TimeOfDay, B.TimeZone)
timetz =
  Value (Value.unsafePTIWithShow PTI.timetz (Prelude.bool A.timetz_float A.timetz_int))

-- |
-- Encoder of @INTERVAL@ values.
{-# INLINABLE interval #-}
interval :: Value B.DiffTime
interval =
  Value (Value.unsafePTIWithShow PTI.interval (Prelude.bool A.interval_float A.interval_int))

-- |
-- Encoder of @UUID@ values.
{-# INLINABLE uuid #-}
uuid :: Value B.UUID
uuid =
  Value (Value.unsafePTIWithShow PTI.uuid (const A.uuid))

-- |
-- Encoder of @INET@ values.
{-# INLINABLE inet #-}
inet :: Value (B.NetAddr B.IP)
inet =
  Value (Value.unsafePTIWithShow PTI.inet (const A.inet))

-- |
-- Encoder of @JSON@ values from JSON AST.
{-# INLINABLE json #-}
json :: Value B.Value
json =
  Value (Value.unsafePTIWithShow PTI.json (const A.json_ast))

-- |
-- Encoder of @JSON@ values from raw JSON.
{-# INLINABLE jsonBytes #-}
jsonBytes :: Value ByteString
jsonBytes =
  Value (Value.unsafePTIWithShow PTI.json (const A.json_bytes))

-- |
-- Encoder of @JSONB@ values from JSON AST.
{-# INLINABLE jsonb #-}
jsonb :: Value B.Value
jsonb =
  Value (Value.unsafePTIWithShow PTI.jsonb (const A.jsonb_ast))

-- |
-- Encoder of @JSONB@ values from raw JSON.
{-# INLINABLE jsonbBytes #-}
jsonbBytes :: Value ByteString
jsonbBytes =
  Value (Value.unsafePTIWithShow PTI.jsonb (const A.jsonb_bytes))

-- |
-- Unlifts the 'Array' encoder to the plain 'Value' encoder.
{-# INLINABLE array #-}
array :: Array a -> Value a
array (Array (Array.Array valueOID arrayOID arrayEncoder renderer)) =
  let
    encoder env input =
      A.array (PTI.oidWord32 valueOID) (arrayEncoder env input)
    in Value (Value.Value arrayOID arrayOID encoder renderer)

-- |
-- Given a function,
-- which maps the value into the textual enum label from the DB side,
-- produces a encoder of that value.
{-# INLINABLE enum #-}
enum :: (a -> Text) -> Value a
enum mapping =
  Value (Value.unsafePTI PTI.text (const (A.text_strict . mapping)) (C.text . mapping))

-- |
-- Identifies the value with the PostgreSQL's \"unknown\" type,
-- thus leaving it up to Postgres to infer the actual type of the value.
-- 
-- The value transimitted is any value encoded in the Postgres' Text data format.
-- For reference, see the
-- <https://www.postgresql.org/docs/10/static/protocol-overview.html#protocol-format-codes Formats and Format Codes>
-- section of the Postgres' documentation.
{-# INLINABLE unknown #-}
unknown :: Value ByteString
unknown =
  Value (Value.unsafePTIWithShow PTI.unknown (const A.bytea_strict))


-- * Array
-------------------------

-- |
-- A generic array encoder.
-- 
-- Here's an example of its usage:
-- 
-- >x :: Value [[Int64]]
-- >x =
-- >  array (dimension foldl' (dimension foldl' (element int8)))
-- 
-- Please note that the PostgreSQL __IN__ keyword does not "accept" an array, but rather a syntactical list of
-- values, thus this encoder is not suited for that. Use a **field** = ANY($1) query instead.
--
newtype Array a =
  Array (Array.Array a)

-- |
-- Lifts the 'Value' encoder into the 'Array' encoder of a non-nullable value.
{-# INLINABLE element #-}
element :: Value a -> Array a
element (Value (Value.Value elementOID arrayOID encoder renderer)) =
  Array (Array.value elementOID arrayOID encoder renderer)

-- |
-- Lifts the 'Value' encoder into the 'Array' encoder of a nullable value.
{-# INLINABLE nullableElement #-}
nullableElement :: Value a -> Array (Maybe a)
nullableElement (Value (Value.Value elementOID arrayOID encoder renderer)) =
  Array (Array.nullableValue elementOID arrayOID encoder renderer)

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
-- * A component encoder, which can be either another 'dimension',
-- 'element' or 'nullableElement'.
-- 
{-# INLINABLE dimension #-}
dimension :: (forall a. (a -> b -> a) -> a -> c -> a) -> Array b -> Array c
dimension foldl (Array imp) =
  Array (Array.dimension foldl imp)

-- $insertMany
-- It is not currently possible to pass in an array of encodable values
-- to use in an 'insert many' statement using Hasql. Instead, PostgreSQL's
--  (9.4 or later) `unnest` function can be used to in an analogous way
-- to haskell's `zip` function by passing in multiple arrays of values
-- to be zipped into the rows we want to insert:
--
-- @
--   insertMultipleLocations :: Statement (Vector (UUID, Double, Double)) ()
--   insertMultipleLocations =
--     statement sql encoder decoder True
--     where
--       sql =
--         "insert into location (id, x, y) select * from unnest ($1, $2, $3)"
--       encoder =
--         contramap Vector.unzip3 $
--         contrazip3 (vector Encoders.uuid) (vector Encoders.float8) (vector Encoders.float8)
--         where
--           vector value =
--             Encoders.param (Encoders.array (Encoders.dimension foldl' (Encoders.element value)))
--       decoder =
--         Decoders.unit
-- @
