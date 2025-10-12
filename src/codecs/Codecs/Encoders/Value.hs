module Codecs.Encoders.Value where

import Codecs.TypeInfo qualified as TypeInfo
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IP qualified as Iproute
import Platform.Prelude
import PostgreSQL.Binary.Encoding qualified as Binary
import PostgreSQL.Binary.Range qualified as Range
import TextBuilder qualified as TextBuilder

-- |
-- Value encoder.
data Value a
  = Value
      -- | Type name.
      Text
      -- | Text format?
      Bool
      -- | Statically known OID for the type.
      -- When unspecified, the OID may be determined at runtime by looking up by name.
      (Maybe Word32)
      -- | Statically known OID for the array-type with this type as the element.
      -- When unspecified, the OID may be determined at runtime by looking up by name.
      -- It may also mean that there may be no array type containing this type, which is the case in attempts to double-nest arrays.
      (Maybe Word32)
      -- | Serialization function (always integer timestamps for PostgreSQL 10+).
      (a -> Binary.Encoding)
      -- | Render function for error messages.
      (a -> TextBuilder.TextBuilder)

instance Contravariant Value where
  {-# INLINE contramap #-}
  contramap f (Value typeName textFormat valueOid arrayOid encode render) =
    Value typeName textFormat valueOid arrayOid (encode . f) (render . f)

{-# INLINE unsafeTypeInfo #-}
unsafeTypeInfo :: TypeInfo.TypeInfo -> (a -> Binary.Encoding) -> (a -> TextBuilder.TextBuilder) -> Value a
unsafeTypeInfo =
  std "unknown" False

{-# INLINE std #-}
std :: Text -> Bool -> TypeInfo.TypeInfo -> (a -> Binary.Encoding) -> (a -> TextBuilder.TextBuilder) -> Value a
std typeName isText typeInfo =
  Value typeName isText (Just (TypeInfo.toBaseOid typeInfo)) (Just (TypeInfo.toArrayOid typeInfo))

-- |
-- Encoder of @BOOL@ values.
{-# INLINEABLE bool #-}
bool :: Value Bool
bool = std "bool" False TypeInfo.bool Binary.bool (TextBuilder.string . show)

-- |
-- Encoder of @INT2@ values.
{-# INLINEABLE int2 #-}
int2 :: Value Int16
int2 = std "int2" False TypeInfo.int2 Binary.int2_int16 (TextBuilder.string . show)

-- |
-- Encoder of @INT4@ values.
{-# INLINEABLE int4 #-}
int4 :: Value Int32
int4 = std "int4" False TypeInfo.int4 Binary.int4_int32 (TextBuilder.string . show)

-- |
-- Encoder of @INT8@ values.
{-# INLINEABLE int8 #-}
int8 :: Value Int64
int8 = std "int8" False TypeInfo.int8 Binary.int8_int64 (TextBuilder.string . show)

-- |
-- Encoder of @FLOAT4@ values.
{-# INLINEABLE float4 #-}
float4 :: Value Float
float4 = std "float4" False TypeInfo.float4 Binary.float4 (TextBuilder.string . show)

-- |
-- Encoder of @FLOAT8@ values.
{-# INLINEABLE float8 #-}
float8 :: Value Double
float8 = std "float8" False TypeInfo.float8 Binary.float8 (TextBuilder.string . show)

-- |
-- Encoder of @NUMERIC@ values.
{-# INLINEABLE numeric #-}
numeric :: Value Scientific
numeric = std "numeric" False TypeInfo.numeric Binary.numeric (TextBuilder.string . show)

-- |
-- Encoder of @CHAR@ values.
--
-- Note that it supports Unicode values and
-- identifies itself under the @TEXT@ OID because of that.
{-# INLINEABLE char #-}
char :: Value Char
char = std "char" False TypeInfo.text Binary.char_utf8 (TextBuilder.string . show)

-- |
-- Encoder of @TEXT@ values.
{-# INLINEABLE text #-}
text :: Value Text
text = std "text" False TypeInfo.text Binary.text_strict (TextBuilder.string . show)

-- |
-- Encoder of @BYTEA@ values.
{-# INLINEABLE bytea #-}
bytea :: Value ByteString
bytea = std "bytea" False TypeInfo.bytea Binary.bytea_strict (TextBuilder.string . show)

-- |
-- Encoder of @DATE@ values.
{-# INLINEABLE date #-}
date :: Value Day
date = std "date" False TypeInfo.date Binary.date (TextBuilder.string . show)

-- |
-- Encoder of @TIMESTAMP@ values.
{-# INLINEABLE timestamp #-}
timestamp :: Value LocalTime
timestamp = std "timestamp" False TypeInfo.timestamp Binary.timestamp_int (TextBuilder.string . show)

-- |
-- Encoder of @TIMESTAMPTZ@ values.
{-# INLINEABLE timestamptz #-}
timestamptz :: Value UTCTime
timestamptz = std "timestamptz" False TypeInfo.timestamptz Binary.timestamptz_int (TextBuilder.string . show)

-- |
-- Encoder of @TIME@ values.
{-# INLINEABLE time #-}
time :: Value TimeOfDay
time = std "time" False TypeInfo.time Binary.time_int (TextBuilder.string . show)

-- |
-- Encoder of @TIMETZ@ values.
{-# INLINEABLE timetz #-}
timetz :: Value (TimeOfDay, TimeZone)
timetz = std "timetz" False TypeInfo.timetz Binary.timetz_int (TextBuilder.string . show)

-- |
-- Encoder of @INTERVAL@ values.
{-# INLINEABLE interval #-}
interval :: Value DiffTime
interval = std "interval" False TypeInfo.interval Binary.interval_int (TextBuilder.string . show)

-- |
-- Encoder of @UUID@ values.
{-# INLINEABLE uuid #-}
uuid :: Value UUID
uuid = std "uuid" False TypeInfo.uuid Binary.uuid (TextBuilder.string . show)

-- |
-- Encoder of @INET@ values.
{-# INLINEABLE inet #-}
inet :: Value Iproute.IPRange
inet = std "inet" False TypeInfo.inet Binary.inet (TextBuilder.string . show)

-- |
-- Encoder of @MACADDR@ values.
--
-- Represented as a 6-tuple of Word8 values in big endian order. If
-- you use `ip` library you can convert using its `toOctets`.
--
-- > toOctets >$< macaddr
{-# INLINEABLE macaddr #-}
macaddr :: Value (Word8, Word8, Word8, Word8, Word8, Word8)
macaddr = std "macaddr" False TypeInfo.macaddr Binary.macaddr (TextBuilder.string . show)

-- |
-- Encoder of @JSON@ values from JSON AST.
{-# INLINEABLE json #-}
json :: Value Aeson.Value
json = std "json" False TypeInfo.json Binary.json_ast (TextBuilder.string . show)

-- |
-- Encoder of @JSON@ values from raw JSON.
{-# INLINEABLE jsonBytes #-}
jsonBytes :: Value ByteString
jsonBytes = std "json" False TypeInfo.json Binary.json_bytes (TextBuilder.string . show)

-- |
-- Encoder of @JSON@ values from raw JSON as lazy ByteString.
{-# INLINEABLE jsonLazyBytes #-}
jsonLazyBytes :: Value LazyByteString.ByteString
jsonLazyBytes = std "json" False TypeInfo.json Binary.json_bytes_lazy (TextBuilder.string . show)

-- |
-- Encoder of @JSONB@ values from JSON AST.
{-# INLINEABLE jsonb #-}
jsonb :: Value Aeson.Value
jsonb = std "jsonb" False TypeInfo.jsonb Binary.jsonb_ast (TextBuilder.string . show)

-- |
-- Encoder of @JSONB@ values from raw JSON.
{-# INLINEABLE jsonbBytes #-}
jsonbBytes :: Value ByteString
jsonbBytes = std "jsonb" False TypeInfo.jsonb Binary.jsonb_bytes (TextBuilder.string . show)

-- |
-- Encoder of @JSONB@ values from raw JSON as lazy ByteString.
{-# INLINEABLE jsonbLazyBytes #-}
jsonbLazyBytes :: Value LazyByteString.ByteString
jsonbLazyBytes = std "jsonb" False TypeInfo.jsonb Binary.jsonb_bytes_lazy (TextBuilder.string . show)

-- |
-- Encoder of @OID@ values.
{-# INLINEABLE oid #-}
oid :: Value Int32
oid = std "oid" False TypeInfo.oid Binary.int4_int32 (TextBuilder.string . show)

-- |
-- Encoder of @NAME@ values.
{-# INLINEABLE name #-}
name :: Value Text
name = std "name" False TypeInfo.name Binary.text_strict (TextBuilder.string . show)

-- |
-- Encoder of @INT4RANGE@ values.
{-# INLINEABLE int4range #-}
int4range :: Value (Range.Range Int32)
int4range = std "int4range" False TypeInfo.int4range Binary.int4range (TextBuilder.string . show)

-- |
-- Encoder of @INT8RANGE@ values.
{-# INLINEABLE int8range #-}
int8range :: Value (Range.Range Int64)
int8range = std "int8range" False TypeInfo.int8range Binary.int8range (TextBuilder.string . show)

-- |
-- Encoder of @NUMRANGE@ values.
{-# INLINEABLE numrange #-}
numrange :: Value (Range.Range Scientific)
numrange = std "numrange" False TypeInfo.numrange Binary.numrange (TextBuilder.string . show)

-- |
-- Encoder of @TSRANGE@ values.
{-# INLINEABLE tsrange #-}
tsrange :: Value (Range.Range LocalTime)
tsrange = std "tsrange" False TypeInfo.tsrange Binary.tsrange_int (TextBuilder.string . show)

-- |
-- Encoder of @TSTZRANGE@ values.
{-# INLINEABLE tstzrange #-}
tstzrange :: Value (Range.Range UTCTime)
tstzrange = std "tstzrange" False TypeInfo.tstzrange Binary.tstzrange_int (TextBuilder.string . show)

-- |
-- Encoder of @DATERANGE@ values.
{-# INLINEABLE daterange #-}
daterange :: Value (Range.Range Day)
daterange = std "daterange" False TypeInfo.daterange Binary.daterange (TextBuilder.string . show)

-- |
-- Encoder of @INT4MULTIRANGE@ values.
{-# INLINEABLE int4multirange #-}
int4multirange :: Value (Range.Multirange Int32)
int4multirange = std "int4multirange" False TypeInfo.int4multirange Binary.int4multirange (TextBuilder.string . show)

-- |
-- Encoder of @INT8MULTIRANGE@ values.
{-# INLINEABLE int8multirange #-}
int8multirange :: Value (Range.Multirange Int64)
int8multirange = std "int8multirange" False TypeInfo.int8multirange Binary.int8multirange (TextBuilder.string . show)

-- |
-- Encoder of @NUMMULTIRANGE@ values.
{-# INLINEABLE nummultirange #-}
nummultirange :: Value (Range.Multirange Scientific)
nummultirange = std "nummultirange" False TypeInfo.nummultirange Binary.nummultirange (TextBuilder.string . show)

-- |
-- Encoder of @TSMULTIRANGE@ values.
{-# INLINEABLE tsmultirange #-}
tsmultirange :: Value (Range.Multirange LocalTime)
tsmultirange = std "tsmultirange" False TypeInfo.tsmultirange Binary.tsmultirange_int (TextBuilder.string . show)

-- |
-- Encoder of @TSTZMULTIRANGE@ values.
{-# INLINEABLE tstzmultirange #-}
tstzmultirange :: Value (Range.Multirange UTCTime)
tstzmultirange = std "tstzmultirange" False TypeInfo.tstzmultirange Binary.tstzmultirange_int (TextBuilder.string . show)

-- |
-- Encoder of @DATEMULTIRANGE@ values.
{-# INLINEABLE datemultirange #-}
datemultirange :: Value (Range.Multirange Day)
datemultirange = std "datemultirange" False TypeInfo.datemultirange Binary.datemultirange (TextBuilder.string . show)

-- |
-- Given a function,
-- which maps a value into a textual enum label used on the DB side,
-- produces an encoder of that value.
{-# INLINEABLE enum #-}
enum :: (a -> Text) -> Value a
enum mapping = std "text" False TypeInfo.text (Binary.text_strict . mapping) (TextBuilder.text . mapping)

-- |
-- Variation of 'enum' with unknown OID.
-- This function does not identify the type to Postgres,
-- so Postgres must be able to derive the type from context.
-- When you find yourself in such situation just provide an explicit type in the query
-- using the :: operator.
{-# INLINEABLE unknownEnum #-}
unknownEnum :: (a -> Text) -> Value a
unknownEnum mapping = std "unknown" False TypeInfo.unknown (Binary.text_strict . mapping) (TextBuilder.text . mapping)

-- |
-- Identifies the value with the PostgreSQL's \"unknown\" type,
-- thus leaving it up to Postgres to infer the actual type of the value.
--
-- The value transimitted is any value encoded in the Postgres' Text data format.
-- For reference, see the
-- <https://www.postgresql.org/docs/10/static/protocol-overview.html#PROTOCOL-FORMAT-CODES Formats and Format Codes>
-- section of the Postgres' documentation.
--
-- __Warning:__ Do not use this as part of composite encoders like 'array' since
-- it is the only encoder that doesn't use the binary format.
{-# INLINEABLE unknown #-}
unknown :: Value ByteString
unknown = std "unknown" True TypeInfo.unknown Binary.bytea_strict (TextBuilder.string . show)
