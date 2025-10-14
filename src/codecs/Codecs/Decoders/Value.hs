module Codecs.Decoders.Value where

import Codecs.TypeInfo qualified as TypeInfo
import Data.Aeson qualified as Aeson
import Data.IP qualified as Iproute
import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as Binary
import PostgreSQL.Binary.Range qualified as R

-- |
-- Value decoder.
data Value a
  = Value
      -- | Schema name.
      (Maybe Text)
      -- | Type name.
      Text
      -- | Statically known OID for the type.
      (Maybe Word32)
      -- | Statically known OID for the array-type with this type as the element.
      (Maybe Word32)
      -- | Decoding function (always integer timestamps for PostgreSQL 10+).
      (Binary.Value a)
  deriving (Functor)

instance Filterable Value where
  {-# INLINE mapMaybe #-}
  mapMaybe fn =
    refine (maybe (Left "Invalid value") Right . fn)

{-# INLINE decoder #-}
decoder :: Binary.Value a -> Value a
decoder aDecoder =
  {-# SCC "decoder" #-}
  Value Nothing "unknown" Nothing Nothing aDecoder

{-# INLINE decoderFn #-}
decoderFn :: (Bool -> ByteString -> Either Text a) -> Value a
decoderFn fn =
  Value
    Nothing
    "unknown"
    Nothing
    Nothing
    (Binary.fn $ fn True) -- Always use integer timestamps

-- |
-- Create a decoder from TypeInfo metadata and a decoding function.
{-# INLINE primitive #-}
primitive :: Text -> TypeInfo.TypeInfo -> Binary.Value a -> Value a
primitive typeName pti intDecoder =
  Value Nothing typeName (Just (TypeInfo.toBaseOid pti)) (Just (TypeInfo.toArrayOid pti)) intDecoder

-- * Static types

-- |
-- Decoder of the @BOOL@ values.
{-# INLINEABLE bool #-}
bool :: Value Bool
bool = primitive "bool" TypeInfo.bool Binary.bool

-- |
-- Decoder of the @INT2@ values.
{-# INLINEABLE int2 #-}
int2 :: Value Int16
int2 = primitive "int2" TypeInfo.int2 Binary.int

-- |
-- Decoder of the @INT4@ values.
{-# INLINEABLE int4 #-}
int4 :: Value Int32
int4 = primitive "int4" TypeInfo.int4 Binary.int

-- |
-- Decoder of the @INT8@ values.
{-# INLINEABLE int8 #-}
int8 :: Value Int64
int8 =
  {-# SCC "int8" #-}
  primitive "int8" TypeInfo.int8 ({-# SCC "int8.int" #-} Binary.int)

-- |
-- Decoder of the @FLOAT4@ values.
{-# INLINEABLE float4 #-}
float4 :: Value Float
float4 = primitive "float4" TypeInfo.float4 Binary.float4

-- |
-- Decoder of the @FLOAT8@ values.
{-# INLINEABLE float8 #-}
float8 :: Value Double
float8 = primitive "float8" TypeInfo.float8 Binary.float8

-- |
-- Decoder of the @NUMERIC@ values.
{-# INLINEABLE numeric #-}
numeric :: Value Scientific
numeric = primitive "numeric" TypeInfo.numeric Binary.numeric

-- |
-- Decoder of the @CHAR@ values.
-- Note that it supports Unicode values.
{-# INLINEABLE char #-}
char :: Value Char
char = primitive "char" TypeInfo.char Binary.char

-- |
-- Decoder of the @TEXT@ values.
{-# INLINEABLE text #-}
text :: Value Text
text = primitive "text" TypeInfo.text Binary.text_strict

-- |
-- Decoder of the @BYTEA@ values.
{-# INLINEABLE bytea #-}
bytea :: Value ByteString
bytea = primitive "bytea" TypeInfo.bytea Binary.bytea_strict

-- |
-- Decoder of the @DATE@ values.
{-# INLINEABLE date #-}
date :: Value Day
date = primitive "date" TypeInfo.date Binary.date

-- |
-- Decoder of the @TIMESTAMP@ values.
{-# INLINEABLE timestamp #-}
timestamp :: Value LocalTime
timestamp = primitive "timestamp" TypeInfo.timestamp Binary.timestamp_int

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
timestamptz = primitive "timestamptz" TypeInfo.timestamptz Binary.timestamptz_int

-- |
-- Decoder of the @TIME@ values.
{-# INLINEABLE time #-}
time :: Value TimeOfDay
time = primitive "time" TypeInfo.time Binary.time_int

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
timetz = primitive "timetz" TypeInfo.timetz Binary.timetz_int

-- |
-- Decoder of the @INTERVAL@ values.
{-# INLINEABLE interval #-}
interval :: Value DiffTime
interval = primitive "interval" TypeInfo.interval Binary.interval_int

-- |
-- Decoder of the @UUID@ values.
{-# INLINEABLE uuid #-}
uuid :: Value UUID
uuid = primitive "uuid" TypeInfo.uuid Binary.uuid

-- |
-- Decoder of the @INET@ values.
{-# INLINEABLE inet #-}
inet :: Value Iproute.IPRange
inet = primitive "inet" TypeInfo.inet Binary.inet

-- |
-- Decoder of the @MACADDR@ values.
--
-- Represented as a 6-tuple of Word8 values in big endian order. If
-- you use `ip` library consider using it with `fromOctets`.
--
-- > (\(a,b,c,d,e,f) -> fromOctets a b c d e f) <$> macaddr
{-# INLINEABLE macaddr #-}
macaddr :: Value (Word8, Word8, Word8, Word8, Word8, Word8)
macaddr = primitive "macaddr" TypeInfo.macaddr Binary.macaddr

-- |
-- Decoder of the @JSON@ values into a JSON AST.
{-# INLINEABLE json #-}
json :: Value Aeson.Value
json = primitive "json" TypeInfo.json Binary.json_ast

-- |
-- Decoder of the @JSON@ values into a raw JSON 'ByteString'.
{-# INLINEABLE jsonBytes #-}
jsonBytes :: (ByteString -> Either Text a) -> Value a
jsonBytes fn = decoder (Binary.json_bytes fn)

-- |
-- Decoder of the @JSONB@ values into a JSON AST.
{-# INLINEABLE jsonb #-}
jsonb :: Value Aeson.Value
jsonb = primitive "jsonb" TypeInfo.jsonb Binary.jsonb_ast

-- |
-- Decoder of the @JSONB@ values into a raw JSON 'ByteString'.
{-# INLINEABLE jsonbBytes #-}
jsonbBytes :: (ByteString -> Either Text a) -> Value a
jsonbBytes fn = decoder (Binary.jsonb_bytes fn)

-- |
-- Decoder of the @INT4RANGE@ values.
{-# INLINEABLE int4range #-}
int4range :: Value (R.Range Int32)
int4range = primitive "int4range" TypeInfo.int4range Binary.int4range

-- |
-- Decoder of the @INT8RANGE@ values.
{-# INLINEABLE int8range #-}
int8range :: Value (R.Range Int64)
int8range = primitive "int8range" TypeInfo.int8range Binary.int8range

-- |
-- Decoder of the @NUMRANGE@ values.
{-# INLINEABLE numrange #-}
numrange :: Value (R.Range Scientific)
numrange = primitive "numrange" TypeInfo.numrange Binary.numrange

-- |
-- Decoder of the @TSRANGE@ values.
{-# INLINEABLE tsrange #-}
tsrange :: Value (R.Range LocalTime)
tsrange = primitive "tsrange" TypeInfo.tsrange Binary.tsrange_int

-- |
-- Decoder of the @TSTZRANGE@ values.
{-# INLINEABLE tstzrange #-}
tstzrange :: Value (R.Range UTCTime)
tstzrange = primitive "tstzrange" TypeInfo.tstzrange Binary.tstzrange_int

-- |
-- Decoder of the @DATERANGE@ values.
{-# INLINEABLE daterange #-}
daterange :: Value (R.Range Day)
daterange = primitive "daterange" TypeInfo.daterange Binary.daterange

-- |
-- Decoder of the @INT4MULTIRANGE@ values.
{-# INLINEABLE int4multirange #-}
int4multirange :: Value (R.Multirange Int32)
int4multirange = primitive "int4multirange" TypeInfo.int4multirange Binary.int4multirange

-- |
-- Decoder of the @INT8MULTIRANGE@ values.
{-# INLINEABLE int8multirange #-}
int8multirange :: Value (R.Multirange Int64)
int8multirange = primitive "int8multirange" TypeInfo.int8multirange Binary.int8multirange

-- |
-- Decoder of the @NUMMULTIRANGE@ values.
{-# INLINEABLE nummultirange #-}
nummultirange :: Value (R.Multirange Scientific)
nummultirange = primitive "nummultirange" TypeInfo.nummultirange Binary.nummultirange

-- |
-- Decoder of the @TSMULTIRANGE@ values.
{-# INLINEABLE tsmultirange #-}
tsmultirange :: Value (R.Multirange LocalTime)
tsmultirange = primitive "tsmultirange" TypeInfo.tsmultirange Binary.tsmultirange_int

-- |
-- Decoder of the @TSTZMULTIRANGE@ values.
{-# INLINEABLE tstzmultirange #-}
tstzmultirange :: Value (R.Multirange UTCTime)
tstzmultirange = primitive "tstzmultirange" TypeInfo.tstzmultirange Binary.tstzmultirange_int

-- |
-- Decoder of the @DATEMULTIRANGE@ values.
{-# INLINEABLE datemultirange #-}
datemultirange :: Value (R.Multirange Day)
datemultirange = primitive "datemultirange" TypeInfo.datemultirange Binary.datemultirange

-- |
-- Lift a custom value decoder function to a 'Value' decoder.
{-# INLINEABLE custom #-}
custom :: (ByteString -> Either Text a) -> Value a
custom fn = decoder (Binary.fn fn)

-- |
-- Refine a value decoder, lifting the possible error to the session level.
{-# INLINE refine #-}
refine :: (a -> Either Text b) -> Value a -> Value b
refine fn (Value schema typeName typeOid arrayOid decoder) =
  Value schema typeName typeOid arrayOid (Binary.refine fn decoder)

-- |
-- Binary generic decoder of @HSTORE@ values.
--
-- Here's how you can use it to construct a specific value:
--
-- @
-- x :: Value [(Text, Maybe Text)]
-- x = hstore 'replicateM'
-- @
{-# INLINEABLE hstore #-}
hstore :: (forall m. (Monad m) => Int -> m (Text, Maybe Text) -> m a) -> Value a
hstore replicateM = decoder (Binary.hstore replicateM Binary.text_strict Binary.text_strict)

-- |
-- Given a partial mapping from text to value,
-- produces a decoder of that value.
enum :: (Text -> Maybe a) -> Value a
enum mapping = decoder (Binary.enum mapping)

-- * Relations

toSchema :: Value a -> Maybe Text
toSchema (Value schema _ _ _ _) = schema

toTypeName :: Value a -> Text
toTypeName (Value _ typeName _ _ _) = typeName

toBaseOid :: Value a -> Maybe Word32
toBaseOid (Value _ _ typeOid arrayOid _) =
  typeOid <|> arrayOid

toArrayOid :: Value a -> Maybe Word32
toArrayOid (Value _ _ _ oid _) = oid

{-# INLINE toHandler #-}
toHandler :: Value a -> Binary.Value a
toHandler (Value _ _ _ _ decoder) = decoder

{-# INLINE toByteStringParser #-}
toByteStringParser :: Value a -> (ByteString -> Either Text a)
toByteStringParser (Value _ _ _ _ decoder) = Binary.valueParser decoder
