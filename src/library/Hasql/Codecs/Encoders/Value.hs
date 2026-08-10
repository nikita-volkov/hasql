module Hasql.Codecs.Encoders.Value where

import ByteString.StrictBuilder qualified
import CodecVocab qualified as CodecVocab
import CodecVocab.QualifiedTypeName qualified as CodecVocab.QualifiedTypeName
import CodecVocab.TypeInfo qualified as CodecVocab.TypeInfo
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IP qualified as Iproute
import Hasql.Platform.Prelude
import Hasql.ToBeResolved qualified as ToBeResolved
import PostgreSQL.Binary.Encoding qualified as Binary
import PostgreSQL.Binary.Range qualified as Range
import TextBuilder qualified as TextBuilder

-- |
-- Value encoder.
data Value a
  = Value
      -- | Schema name, if any.
      (Maybe Text)
      -- | Type name.
      Text
      -- | Statically known OID for the type.
      -- When unspecified, the OID may be determined at runtime by looking up by name.
      (Maybe Word32)
      -- | Statically known OID for the array-type with this type as the element.
      -- When unspecified, the OID may be determined at runtime by looking up by name.
      -- It may also mean that there may be no array type containing this type, which is the case in attempts to double-nest arrays.
      (Maybe Word32)
      -- | Dimensionality. If 0 then it is not an array, but a scalar value.
      Word
      -- | Text format?
      Bool
      -- | Serialization function, deferring the names of types that must be looked up at runtime.
      (ToBeResolved.ToBeResolved CodecVocab.QualifiedTypeName CodecVocab.TypeInfo (a -> Binary.Encoding))
      -- | Render function for error messages.
      (a -> TextBuilder.TextBuilder)

instance Contravariant Value where
  {-# INLINE contramap #-}
  contramap f (Value schemaName typeName valueOid arrayOid dimensionality textFormat serialize render) =
    Value schemaName typeName valueOid arrayOid dimensionality textFormat (fmap (\encode -> encode . f) serialize) (render . f)

{-# INLINE primitive #-}
primitive :: Text -> Bool -> CodecVocab.TypeInfo -> (a -> Binary.Encoding) -> (a -> TextBuilder.TextBuilder) -> Value a
primitive typeName isText typeInfo encode render =
  Value
    Nothing
    typeName
    (Just (CodecVocab.TypeInfo.toBaseOid typeInfo))
    (Just (CodecVocab.TypeInfo.toArrayOid typeInfo))
    0
    isText
    (pure encode)
    render

-- |
-- Encoder of @BOOL@ values.
{-# INLINEABLE bool #-}
bool :: Value Bool
bool = primitive "bool" False CodecVocab.TypeInfo.bool Binary.bool (TextBuilder.string . show)

-- |
-- Encoder of @INT2@ values.
{-# INLINEABLE int2 #-}
int2 :: Value Int16
int2 = primitive "int2" False CodecVocab.TypeInfo.int2 Binary.int2_int16 (TextBuilder.string . show)

-- |
-- Encoder of @INT4@ values.
{-# INLINEABLE int4 #-}
int4 :: Value Int32
int4 = primitive "int4" False CodecVocab.TypeInfo.int4 Binary.int4_int32 (TextBuilder.string . show)

-- |
-- Encoder of @INT8@ values.
{-# INLINEABLE int8 #-}
int8 :: Value Int64
int8 = primitive "int8" False CodecVocab.TypeInfo.int8 Binary.int8_int64 (TextBuilder.string . show)

-- |
-- Encoder of @FLOAT4@ values.
{-# INLINEABLE float4 #-}
float4 :: Value Float
float4 = primitive "float4" False CodecVocab.TypeInfo.float4 Binary.float4 (TextBuilder.string . show)

-- |
-- Encoder of @FLOAT8@ values.
{-# INLINEABLE float8 #-}
float8 :: Value Double
float8 = primitive "float8" False CodecVocab.TypeInfo.float8 Binary.float8 (TextBuilder.string . show)

-- |
-- Encoder of @NUMERIC@ values.
{-# INLINEABLE numeric #-}
numeric :: Value Scientific
numeric = primitive "numeric" False CodecVocab.TypeInfo.numeric Binary.numeric (TextBuilder.string . show)

-- |
-- Encoder of @CHAR@ values.
--
-- Note that it supports Unicode values and
-- identifies itself under the @TEXT@ OID because of that.
{-# INLINEABLE char #-}
char :: Value Char
char = primitive "char" False CodecVocab.TypeInfo.text Binary.char_utf8 (TextBuilder.string . show)

-- |
-- Encoder of @TEXT@ values.
{-# INLINEABLE text #-}
text :: Value Text
text = primitive "text" False CodecVocab.TypeInfo.text Binary.text_strict (TextBuilder.string . show)

-- |
-- Encoder of @VARCHAR@ values.
{-# INLINEABLE varchar #-}
varchar :: Value Text
varchar = primitive "varchar" False CodecVocab.TypeInfo.varchar Binary.text_strict (TextBuilder.string . show)

-- |
-- Encoder of @BPCHAR@ or @CHAR(n)@, @CHARACTER(n)@ values.
{-# INLINEABLE bpchar #-}
bpchar :: Value Text
bpchar = primitive "bpchar" False CodecVocab.TypeInfo.bpchar Binary.text_strict (TextBuilder.string . show)

-- |
-- Encoder of @BYTEA@ values.
{-# INLINEABLE bytea #-}
bytea :: Value ByteString
bytea = primitive "bytea" False CodecVocab.TypeInfo.bytea Binary.bytea_strict (TextBuilder.string . show)

-- |
-- Encoder of @DATE@ values.
{-# INLINEABLE date #-}
date :: Value Day
date = primitive "date" False CodecVocab.TypeInfo.date Binary.date (TextBuilder.string . show)

-- |
-- Encoder of @TIMESTAMP@ values.
{-# INLINEABLE timestamp #-}
timestamp :: Value LocalTime
timestamp = primitive "timestamp" False CodecVocab.TypeInfo.timestamp Binary.timestamp_int (TextBuilder.string . show)

-- |
-- Encoder of @TIMESTAMPTZ@ values.
{-# INLINEABLE timestamptz #-}
timestamptz :: Value UTCTime
timestamptz = primitive "timestamptz" False CodecVocab.TypeInfo.timestamptz Binary.timestamptz_int (TextBuilder.string . show)

-- |
-- Encoder of @TIME@ values.
{-# INLINEABLE time #-}
time :: Value TimeOfDay
time = primitive "time" False CodecVocab.TypeInfo.time Binary.time_int (TextBuilder.string . show)

-- |
-- Encoder of @TIMETZ@ values.
{-# INLINEABLE timetz #-}
timetz :: Value (TimeOfDay, TimeZone)
timetz = primitive "timetz" False CodecVocab.TypeInfo.timetz Binary.timetz_int (TextBuilder.string . show)

-- |
-- Encoder of @INTERVAL@ values.
{-# INLINEABLE interval #-}
interval :: Value DiffTime
interval = primitive "interval" False CodecVocab.TypeInfo.interval Binary.interval_int (TextBuilder.string . show)

-- |
-- Encoder of @UUID@ values.
{-# INLINEABLE uuid #-}
uuid :: Value UUID
uuid = primitive "uuid" False CodecVocab.TypeInfo.uuid Binary.uuid (TextBuilder.string . show)

-- |
-- Encoder of @INET@ values.
{-# INLINEABLE inet #-}
inet :: Value Iproute.IPRange
inet = primitive "inet" False CodecVocab.TypeInfo.inet Binary.inet (TextBuilder.string . show)

-- |
-- Encoder of @MACADDR@ values.
--
-- Represented as a 6-tuple of Word8 values in big endian order. If
-- you use `ip` library you can convert using its `toOctets`.
--
-- > toOctets >$< macaddr
{-# INLINEABLE macaddr #-}
macaddr :: Value (Word8, Word8, Word8, Word8, Word8, Word8)
macaddr = primitive "macaddr" False CodecVocab.TypeInfo.macaddr Binary.macaddr (TextBuilder.string . show)

-- |
-- Encoder of @JSON@ values from JSON AST.
{-# INLINEABLE json #-}
json :: Value Aeson.Value
json = primitive "json" False CodecVocab.TypeInfo.json Binary.json_ast (TextBuilder.string . show)

-- |
-- Encoder of @JSON@ values from raw JSON.
{-# INLINEABLE jsonBytes #-}
jsonBytes :: Value ByteString
jsonBytes = primitive "json" False CodecVocab.TypeInfo.json Binary.json_bytes (TextBuilder.string . show)

-- |
-- Encoder of @JSON@ values from raw JSON as lazy ByteString.
{-# INLINEABLE jsonLazyBytes #-}
jsonLazyBytes :: Value LazyByteString.ByteString
jsonLazyBytes = primitive "json" False CodecVocab.TypeInfo.json Binary.json_bytes_lazy (TextBuilder.string . show)

-- |
-- Encoder of @JSONB@ values from JSON AST.
{-# INLINEABLE jsonb #-}
jsonb :: Value Aeson.Value
jsonb = primitive "jsonb" False CodecVocab.TypeInfo.jsonb Binary.jsonb_ast (TextBuilder.string . show)

-- |
-- Encoder of @JSONB@ values from raw JSON.
{-# INLINEABLE jsonbBytes #-}
jsonbBytes :: Value ByteString
jsonbBytes = primitive "jsonb" False CodecVocab.TypeInfo.jsonb Binary.jsonb_bytes (TextBuilder.string . show)

-- |
-- Encoder of @JSONB@ values from raw JSON as lazy ByteString.
{-# INLINEABLE jsonbLazyBytes #-}
jsonbLazyBytes :: Value LazyByteString.ByteString
jsonbLazyBytes = primitive "jsonb" False CodecVocab.TypeInfo.jsonb Binary.jsonb_bytes_lazy (TextBuilder.string . show)

-- |
-- Encoder of @OID@ values.
{-# INLINEABLE oid #-}
oid :: Value Int32
oid = primitive "oid" False CodecVocab.TypeInfo.oid Binary.int4_int32 (TextBuilder.string . show)

-- |
-- Encoder of @NAME@ values.
{-# INLINEABLE name #-}
name :: Value Text
name = primitive "name" False CodecVocab.TypeInfo.name Binary.text_strict (TextBuilder.string . show)

-- |
-- Encoder of @INT4RANGE@ values.
{-# INLINEABLE int4range #-}
int4range :: Value (Range.Range Int32)
int4range = primitive "int4range" False CodecVocab.TypeInfo.int4range Binary.int4range (TextBuilder.string . show)

-- |
-- Encoder of @INT8RANGE@ values.
{-# INLINEABLE int8range #-}
int8range :: Value (Range.Range Int64)
int8range = primitive "int8range" False CodecVocab.TypeInfo.int8range Binary.int8range (TextBuilder.string . show)

-- |
-- Encoder of @NUMRANGE@ values.
{-# INLINEABLE numrange #-}
numrange :: Value (Range.Range Scientific)
numrange = primitive "numrange" False CodecVocab.TypeInfo.numrange Binary.numrange (TextBuilder.string . show)

-- |
-- Encoder of @TSRANGE@ values.
{-# INLINEABLE tsrange #-}
tsrange :: Value (Range.Range LocalTime)
tsrange = primitive "tsrange" False CodecVocab.TypeInfo.tsrange Binary.tsrange_int (TextBuilder.string . show)

-- |
-- Encoder of @TSTZRANGE@ values.
{-# INLINEABLE tstzrange #-}
tstzrange :: Value (Range.Range UTCTime)
tstzrange = primitive "tstzrange" False CodecVocab.TypeInfo.tstzrange Binary.tstzrange_int (TextBuilder.string . show)

-- |
-- Encoder of @DATERANGE@ values.
{-# INLINEABLE daterange #-}
daterange :: Value (Range.Range Day)
daterange = primitive "daterange" False CodecVocab.TypeInfo.daterange Binary.daterange (TextBuilder.string . show)

-- |
-- Encoder of @INT4MULTIRANGE@ values.
{-# INLINEABLE int4multirange #-}
int4multirange :: Value (Range.Multirange Int32)
int4multirange = primitive "int4multirange" False CodecVocab.TypeInfo.int4multirange Binary.int4multirange (TextBuilder.string . show)

-- |
-- Encoder of @INT8MULTIRANGE@ values.
{-# INLINEABLE int8multirange #-}
int8multirange :: Value (Range.Multirange Int64)
int8multirange = primitive "int8multirange" False CodecVocab.TypeInfo.int8multirange Binary.int8multirange (TextBuilder.string . show)

-- |
-- Encoder of @NUMMULTIRANGE@ values.
{-# INLINEABLE nummultirange #-}
nummultirange :: Value (Range.Multirange Scientific)
nummultirange = primitive "nummultirange" False CodecVocab.TypeInfo.nummultirange Binary.nummultirange (TextBuilder.string . show)

-- |
-- Encoder of @TSMULTIRANGE@ values.
{-# INLINEABLE tsmultirange #-}
tsmultirange :: Value (Range.Multirange LocalTime)
tsmultirange = primitive "tsmultirange" False CodecVocab.TypeInfo.tsmultirange Binary.tsmultirange_int (TextBuilder.string . show)

-- |
-- Encoder of @TSTZMULTIRANGE@ values.
{-# INLINEABLE tstzmultirange #-}
tstzmultirange :: Value (Range.Multirange UTCTime)
tstzmultirange = primitive "tstzmultirange" False CodecVocab.TypeInfo.tstzmultirange Binary.tstzmultirange_int (TextBuilder.string . show)

-- |
-- Encoder of @DATEMULTIRANGE@ values.
{-# INLINEABLE datemultirange #-}
datemultirange :: Value (Range.Multirange Day)
datemultirange = primitive "datemultirange" False CodecVocab.TypeInfo.datemultirange Binary.datemultirange (TextBuilder.string . show)

-- |
-- Encoder of @CITEXT@ values.
--
-- Requires the @citext@ extension to be installed in the database.
{-# INLINEABLE citext #-}
citext :: Value Text
citext =
  Value
    Nothing
    "citext"
    Nothing
    Nothing
    0
    False
    (pure Binary.text_strict)
    (TextBuilder.string . show)

-- |
-- Given a function which maps a value into a textual enum label used on the DB side,
-- produces an encoder of that value for a named enum type.
{-# INLINEABLE enum #-}
enum ::
  -- | Schema name where the enum type is defined.
  Maybe Text ->
  -- | Enum type name.
  Text ->
  -- | Mapping function from value to enum label.
  (a -> Text) ->
  Value a
enum schemaName typeName mapping =
  Value
    schemaName
    typeName
    Nothing
    Nothing
    0
    False
    (fmap (\_typeInfo -> Binary.text_strict . mapping) (ToBeResolved.lookup (CodecVocab.QualifiedTypeName schemaName typeName)))
    (TextBuilder.text . mapping)

-- |
-- Identifies the value with the PostgreSQL's \"unknown\" type,
-- thus leaving it up to Postgres to infer the actual type of the value.
--
-- The value transmitted is any value encoded in the Postgres' Text data format.
-- For reference, see the
-- <https://www.postgresql.org/docs/10/static/protocol-overview.html#PROTOCOL-FORMAT-CODES Formats and Format Codes>
-- section of the Postgres' documentation.
--
-- __Warning:__ Do not use this as part of composite encoders like 'array' since
-- it is the only encoder that doesn't use the binary format.
{-# DEPRECATED unknown "Use 'custom' instead." #-}
{-# INLINEABLE unknown #-}
unknown :: Value ByteString
unknown = primitive "unknown" True CodecVocab.TypeInfo.unknown Binary.bytea_strict (TextBuilder.string . show)

-- |
-- Low level API for defining custom value encoders.
{-# INLINEABLE custom #-}
custom ::
  -- | Schema name where the type is defined.
  Maybe Text ->
  -- | Type name.
  Text ->
  -- | Possible static OIDs for the type. The first is for scalar values the second is for arrays.
  --
  -- When unspecified, the OIDs will be automatically determined at runtime by looking up by name.
  Maybe (Word32, Word32) ->
  -- | Other named types whose OIDs are needed for serializing.
  --
  -- E.g., when encoding composite types Postgres requires specifying OIDs of all of its fields.
  --
  -- When any of the requested types is missing in the database an error will be raised upon the statement execution.
  [(Maybe Text, Text)] ->
  -- | Serialization function in the context of resolved OIDs of the types requested in the previous parameter.
  --
  -- It's safe to assume that all of the requested types will be present.
  -- In case you run the provided lookup function with unmentioned type names it will produce OID of 0 for them, standing for unknown type in Postgres.
  ( ((Maybe Text, Text) -> (Word32, Word32)) ->
    a ->
    ByteString
  ) ->
  -- | Render function for error messages.
  (a -> Text) ->
  Value a
custom schemaName typeName staticOids requiredTypes encode render =
  Value
    schemaName
    typeName
    (fmap fst staticOids)
    (fmap snd staticOids)
    0
    False
    ( ToBeResolved.ToBeResolved
        (fmap CodecVocab.QualifiedTypeName.fromNameTuple requiredTypes)
        ( \resolve ->
            ByteString.StrictBuilder.bytes
              . encode
                ( \name ->
                    let typeInfo = resolve (CodecVocab.QualifiedTypeName.fromNameTuple name)
                     in (CodecVocab.TypeInfo.toBaseOid typeInfo, CodecVocab.TypeInfo.toArrayOid typeInfo)
                )
        )
    )
    (TextBuilder.text . render)

-- |
-- Encoder of @HSTORE@ values from a foldable container of key-value pairs.
--
-- The value part can be @Nothing@ to represent a NULL value in the hstore.
{-# INLINEABLE hstore #-}
hstore :: (Foldable foldable) => Value (foldable (Text, Maybe Text))
hstore =
  Value
    Nothing
    "hstore"
    Nothing
    Nothing
    0
    False
    (pure Binary.hStore_foldable)
    renderHstore
  where
    renderHstore items =
      mconcat
        [ TextBuilder.text "hstore(",
          TextBuilder.intercalate ", " (foldMap (\pair -> [renderPair pair]) items),
          TextBuilder.text ")"
        ]
    renderPair (key, maybeValue) =
      mconcat
        [ TextBuilder.text "(",
          TextBuilder.string (show key),
          TextBuilder.text ", ",
          case maybeValue of
            Nothing -> TextBuilder.text "NULL"
            Just value -> TextBuilder.string (show value),
          TextBuilder.text ")"
        ]
