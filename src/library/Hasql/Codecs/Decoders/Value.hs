module Hasql.Codecs.Decoders.Value
  ( Value (..),
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
    custom,
    refine,
    hstore,
    enum,
    toDimensionality,
    toDecoder,
    toSchema,
    toTypeName,
    toOid,
    toBaseOid,
    toArrayOid,
    isArray,
  )
where

import Data.Aeson qualified as Aeson
import Data.IP qualified as Iproute
import Hasql.CodecsCore.QualifiedTypeName qualified as CodecsCore.QualifiedTypeName
import Hasql.CodecsCore.TypeInfo qualified as CodecsCore.TypeInfo
import Hasql.Platform.Prelude hiding (bool)
import Hasql.ToBeResolved qualified as ToBeResolved
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
      -- | Dimensionality. If 0 then it is a scalar value, otherwise it is an array with that many dimensions.
      Word
      -- | Decoding function on a registry of OIDs by type name.
      (ToBeResolved.ToBeResolved CodecsCore.QualifiedTypeName.QualifiedTypeName CodecsCore.TypeInfo.TypeInfo (Binary.Value a))
  deriving (Functor)

type role Value representational

instance Filterable Value where
  {-# INLINE mapMaybe #-}
  mapMaybe fn =
    refine (maybe (Left "Invalid value") Right . fn)

-- |
-- Create a decoder from TypeInfo metadata and a decoding function.
{-# INLINE primitive #-}
primitive :: Text -> CodecsCore.TypeInfo.TypeInfo -> Binary.Value a -> Value a
primitive typeName pti decoder =
  Value Nothing typeName (Just (CodecsCore.TypeInfo.toBaseOid pti)) (Just (CodecsCore.TypeInfo.toArrayOid pti)) 0 (pure decoder)

-- * Static types

-- |
-- Decoder of the @BOOL@ values.
{-# INLINEABLE bool #-}
bool :: Value Bool
bool = primitive "bool" CodecsCore.TypeInfo.bool Binary.bool

-- |
-- Decoder of the @INT2@ values.
{-# INLINEABLE int2 #-}
int2 :: Value Int16
int2 = primitive "int2" CodecsCore.TypeInfo.int2 Binary.int

-- |
-- Decoder of the @INT4@ values.
{-# INLINEABLE int4 #-}
int4 :: Value Int32
int4 = primitive "int4" CodecsCore.TypeInfo.int4 Binary.int

-- |
-- Decoder of the @INT8@ values.
{-# INLINEABLE int8 #-}
int8 :: Value Int64
int8 =
  {-# SCC "int8" #-}
  primitive "int8" CodecsCore.TypeInfo.int8 ({-# SCC "int8.int" #-} Binary.int)

-- |
-- Decoder of the @FLOAT4@ values.
{-# INLINEABLE float4 #-}
float4 :: Value Float
float4 = primitive "float4" CodecsCore.TypeInfo.float4 Binary.float4

-- |
-- Decoder of the @FLOAT8@ values.
{-# INLINEABLE float8 #-}
float8 :: Value Double
float8 = primitive "float8" CodecsCore.TypeInfo.float8 Binary.float8

-- |
-- Decoder of the @NUMERIC@ values.
{-# INLINEABLE numeric #-}
numeric :: Value Scientific
numeric = primitive "numeric" CodecsCore.TypeInfo.numeric Binary.numeric

-- |
-- Decoder of the @CHAR@ values.
-- Note that it supports Unicode values.
{-# INLINEABLE char #-}
char :: Value Char
char = primitive "char" CodecsCore.TypeInfo.char Binary.char

-- |
-- Decoder of the @TEXT@ values.
{-# INLINEABLE text #-}
text :: Value Text
text = primitive "text" CodecsCore.TypeInfo.text Binary.text_strict

-- |
-- Decoder of the @VARCHAR@ values.
{-# INLINEABLE varchar #-}
varchar :: Value Text
varchar = primitive "varchar" CodecsCore.TypeInfo.varchar Binary.text_strict

-- |
-- Decoder of @BPCHAR@ or @CHAR(n)@, @CHARACTER(n)@ values.
{-# INLINEABLE bpchar #-}
bpchar :: Value Text
bpchar = primitive "bpchar" CodecsCore.TypeInfo.bpchar Binary.text_strict

-- |
-- Decoder of the @BYTEA@ values.
{-# INLINEABLE bytea #-}
bytea :: Value ByteString
bytea = primitive "bytea" CodecsCore.TypeInfo.bytea Binary.bytea_strict

-- |
-- Decoder of the @DATE@ values.
{-# INLINEABLE date #-}
date :: Value Day
date = primitive "date" CodecsCore.TypeInfo.date Binary.date

-- |
-- Decoder of the @TIMESTAMP@ values.
{-# INLINEABLE timestamp #-}
timestamp :: Value LocalTime
timestamp = primitive "timestamp" CodecsCore.TypeInfo.timestamp Binary.timestamp_int

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
timestamptz = primitive "timestamptz" CodecsCore.TypeInfo.timestamptz Binary.timestamptz_int

-- |
-- Decoder of the @TIME@ values.
{-# INLINEABLE time #-}
time :: Value TimeOfDay
time = primitive "time" CodecsCore.TypeInfo.time Binary.time_int

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
timetz = primitive "timetz" CodecsCore.TypeInfo.timetz Binary.timetz_int

-- |
-- Decoder of the @INTERVAL@ values.
{-# INLINEABLE interval #-}
interval :: Value DiffTime
interval = primitive "interval" CodecsCore.TypeInfo.interval Binary.interval_int

-- |
-- Decoder of the @UUID@ values.
{-# INLINEABLE uuid #-}
uuid :: Value UUID
uuid = primitive "uuid" CodecsCore.TypeInfo.uuid Binary.uuid

-- |
-- Decoder of the @INET@ values.
{-# INLINEABLE inet #-}
inet :: Value Iproute.IPRange
inet = primitive "inet" CodecsCore.TypeInfo.inet Binary.inet

-- |
-- Decoder of the @MACADDR@ values.
--
-- Represented as a 6-tuple of Word8 values in big endian order. If
-- you use `ip` library consider using it with `fromOctets`.
--
-- > (\(a,b,c,d,e,f) -> fromOctets a b c d e f) <$> macaddr
{-# INLINEABLE macaddr #-}
macaddr :: Value (Word8, Word8, Word8, Word8, Word8, Word8)
macaddr = primitive "macaddr" CodecsCore.TypeInfo.macaddr Binary.macaddr

-- |
-- Decoder of the @JSON@ values into a JSON AST.
{-# INLINEABLE json #-}
json :: Value Aeson.Value
json = primitive "json" CodecsCore.TypeInfo.json Binary.json_ast

-- |
-- Decoder of the @JSON@ values into a raw JSON 'ByteString'.
{-# INLINEABLE jsonBytes #-}
jsonBytes :: (ByteString -> Either Text a) -> Value a
jsonBytes fn = primitive "json" CodecsCore.TypeInfo.json (Binary.json_bytes fn)

-- |
-- Decoder of the @JSONB@ values into a JSON AST.
{-# INLINEABLE jsonb #-}
jsonb :: Value Aeson.Value
jsonb = primitive "jsonb" CodecsCore.TypeInfo.jsonb Binary.jsonb_ast

-- |
-- Decoder of the @JSONB@ values into a raw JSON 'ByteString'.
{-# INLINEABLE jsonbBytes #-}
jsonbBytes :: (ByteString -> Either Text a) -> Value a
jsonbBytes fn = primitive "jsonb" CodecsCore.TypeInfo.jsonb (Binary.jsonb_bytes fn)

-- |
-- Decoder of the @INT4RANGE@ values.
{-# INLINEABLE int4range #-}
int4range :: Value (R.Range Int32)
int4range = primitive "int4range" CodecsCore.TypeInfo.int4range Binary.int4range

-- |
-- Decoder of the @INT8RANGE@ values.
{-# INLINEABLE int8range #-}
int8range :: Value (R.Range Int64)
int8range = primitive "int8range" CodecsCore.TypeInfo.int8range Binary.int8range

-- |
-- Decoder of the @NUMRANGE@ values.
{-# INLINEABLE numrange #-}
numrange :: Value (R.Range Scientific)
numrange = primitive "numrange" CodecsCore.TypeInfo.numrange Binary.numrange

-- |
-- Decoder of the @TSRANGE@ values.
{-# INLINEABLE tsrange #-}
tsrange :: Value (R.Range LocalTime)
tsrange = primitive "tsrange" CodecsCore.TypeInfo.tsrange Binary.tsrange_int

-- |
-- Decoder of the @TSTZRANGE@ values.
{-# INLINEABLE tstzrange #-}
tstzrange :: Value (R.Range UTCTime)
tstzrange = primitive "tstzrange" CodecsCore.TypeInfo.tstzrange Binary.tstzrange_int

-- |
-- Decoder of the @DATERANGE@ values.
{-# INLINEABLE daterange #-}
daterange :: Value (R.Range Day)
daterange = primitive "daterange" CodecsCore.TypeInfo.daterange Binary.daterange

-- |
-- Decoder of the @INT4MULTIRANGE@ values.
{-# INLINEABLE int4multirange #-}
int4multirange :: Value (R.Multirange Int32)
int4multirange = primitive "int4multirange" CodecsCore.TypeInfo.int4multirange Binary.int4multirange

-- |
-- Decoder of the @INT8MULTIRANGE@ values.
{-# INLINEABLE int8multirange #-}
int8multirange :: Value (R.Multirange Int64)
int8multirange = primitive "int8multirange" CodecsCore.TypeInfo.int8multirange Binary.int8multirange

-- |
-- Decoder of the @NUMMULTIRANGE@ values.
{-# INLINEABLE nummultirange #-}
nummultirange :: Value (R.Multirange Scientific)
nummultirange = primitive "nummultirange" CodecsCore.TypeInfo.nummultirange Binary.nummultirange

-- |
-- Decoder of the @TSMULTIRANGE@ values.
{-# INLINEABLE tsmultirange #-}
tsmultirange :: Value (R.Multirange LocalTime)
tsmultirange = primitive "tsmultirange" CodecsCore.TypeInfo.tsmultirange Binary.tsmultirange_int

-- |
-- Decoder of the @TSTZMULTIRANGE@ values.
{-# INLINEABLE tstzmultirange #-}
tstzmultirange :: Value (R.Multirange UTCTime)
tstzmultirange = primitive "tstzmultirange" CodecsCore.TypeInfo.tstzmultirange Binary.tstzmultirange_int

-- |
-- Decoder of the @DATEMULTIRANGE@ values.
{-# INLINEABLE datemultirange #-}
datemultirange :: Value (R.Multirange Day)
datemultirange = primitive "datemultirange" CodecsCore.TypeInfo.datemultirange Binary.datemultirange

-- |
-- Decoder of the @CITEXT@ values.
--
-- Requires the @citext@ extension to be installed in the database.
{-# INLINEABLE citext #-}
citext :: Value Text
citext = Value Nothing "citext" Nothing Nothing 0 (pure Binary.text_strict)

-- |
-- Low level API for defining custom value decoders.
{-# INLINEABLE custom #-}
custom ::
  -- | Schema name.
  Maybe Text ->
  -- | Type name.
  Text ->
  -- | Possible static OIDs for the type. The first is for scalar values the second is for arrays.
  --
  -- When unspecified, the OIDs will be automatically determined at runtime by looking up by name.
  Maybe (Word32, Word32) ->
  -- | Other named types whose OIDs are needed for deserializing.
  --
  -- E.g., when decoding composite types you can check the OIDs of its fields against the ones specified by Postgres.
  --
  -- When any of the requested types is missing in the database an error will be raised upon the statement execution.
  [(Maybe Text, Text)] ->
  -- | Deserialization function in the context of resolved OIDs of the types requested in the previous parameter.
  --
  -- It's safe to assume that all of the requested types will be present.
  -- In case you run the provided lookup function with unmentioned type names it will produce OID of 0 for them, standing for unknown type in Postgres.
  ( ((Maybe Text, Text) -> (Word32, Word32)) ->
    ByteString ->
    Either Text a
  ) ->
  Value a
custom schema typeName staticOids requestedTypes fn =
  Value
    schema
    typeName
    (fmap fst staticOids)
    (fmap snd staticOids)
    0
    (ToBeResolved.ToBeResolved (fmap CodecsCore.QualifiedTypeName.fromNameTuple requestedTypes) (\lookup -> Binary.fn (fn (toTuple . lookup . CodecsCore.QualifiedTypeName.fromNameTuple))))
  where
    toTuple typeInfo = (CodecsCore.TypeInfo.toBaseOid typeInfo, CodecsCore.TypeInfo.toArrayOid typeInfo)

-- |
-- Refine a value decoder, lifting the possible error to the session level.
{-# INLINE refine #-}
refine :: (a -> Either Text b) -> Value a -> Value b
refine fn (Value schema typeName typeOid arrayOid dimensionality decoder) =
  Value schema typeName typeOid arrayOid dimensionality (fmap (Binary.refine fn) decoder)

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
hstore replicateM =
  Value Nothing "hstore" Nothing Nothing 0 (pure (Binary.hstore replicateM Binary.text_strict Binary.text_strict))

-- |
-- Given a partial mapping from text to value, produces a decoder of that value for a named enum type.
enum ::
  -- | Schema name.
  Maybe Text ->
  -- | Type name.
  Text ->
  -- | Mapping from text to value.
  (Text -> Maybe a) ->
  Value a
enum schema typeName mapping =
  Value schema typeName Nothing Nothing 0 (pure (Binary.enum mapping))

-- * Relations

toDimensionality :: Value a -> Word
toDimensionality (Value _ _ _ _ dimensionality _) = dimensionality

toSchema :: Value a -> Maybe Text
toSchema (Value schema _ _ _ _ _) = schema

toTypeName :: Value a -> Text
toTypeName (Value _ typeName _ _ _ _) = typeName

toOid :: Value a -> Maybe Word32
toOid (Value _ _ baseOid arrayOid dimensionality _) =
  if dimensionality > 0
    then arrayOid
    else baseOid

toBaseOid :: Value a -> Maybe Word32
toBaseOid (Value _ _ baseOid _ _ _) = baseOid

toArrayOid :: Value a -> Maybe Word32
toArrayOid (Value _ _ _ oid _ _) = oid

toDecoder :: Value a -> ToBeResolved.ToBeResolved CodecsCore.QualifiedTypeName.QualifiedTypeName CodecsCore.TypeInfo.TypeInfo (Binary.Value a)
toDecoder (Value _ _ _ _ _ decoder) = decoder

isArray :: Value a -> Bool
isArray (Value _ _ _ _ dimensionality _) = dimensionality > 0
