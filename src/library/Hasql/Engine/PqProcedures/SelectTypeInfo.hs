module Hasql.Engine.PqProcedures.SelectTypeInfo
  ( SelectTypeInfo (..),
    run,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Hasql.CodecsVocab qualified as CodecsVocab
import Hasql.CodecsVocab.QualifiedTypeName qualified as CodecsVocab.QualifiedTypeName
import Hasql.CodecsVocab.TypeInfo qualified as CodecsVocab.TypeInfo
import Hasql.Comms.ResultDecoder qualified
import Hasql.Comms.Roundtrip qualified
import Hasql.Comms.RowDecoder qualified
import Hasql.Engine.Errors qualified as Errors
import Hasql.Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as BinaryDecoding
import PostgreSQL.Binary.Encoding qualified as Binary
import Pqi qualified as Pq

newtype SelectTypeInfo = SelectTypeInfo
  { -- | Set of (schema name, type name) pairs to look up.
    keys :: HashSet CodecsVocab.QualifiedTypeName
  }

-- | Result maps (schema name, type name) pairs to TypeInfo (scalar OID, array OID).
type SelectTypeInfoResult =
  HashMap CodecsVocab.QualifiedTypeName CodecsVocab.TypeInfo.TypeInfo

run :: Pq.Connection -> SelectTypeInfo -> IO (Either Errors.SessionError SelectTypeInfoResult)
run connection (SelectTypeInfo keys) =
  if HashSet.null keys
    then pure (Right HashMap.empty)
    else
      first Errors.fromRoundtripError
        <$> Hasql.Comms.Roundtrip.toSerialIO (roundtrip (SelectTypeInfo keys)) connection

sql :: ByteString
sql =
  "with\n\
  \  inputs as (\n\
  \    select *\n\
  \    from unnest($1, $2) as x(schema_name, type_name)\n\
  \  ),\n\
  \  unnamespaced_results as (\n\
  \    select\n\
  \      null :: text as schema_name,\n\
  \      pg_type.typname :: text as type_name,\n\
  \      pg_type.oid :: int4 as type_oid,\n\
  \      pg_type.typarray :: int4 as array_oid\n\
  \    from inputs\n\
  \    join pg_type on pg_type.oid = to_regtype(inputs.type_name)\n\
  \    where inputs.schema_name is null\n\
  \  ),\n\
  \  namespaced_results as (\n\
  \    select\n\
  \      pg_namespace.nspname :: text as schema_name,\n\
  \      pg_type.typname :: text as type_name,\n\
  \      pg_type.oid :: int4 as type_oid,\n\
  \      pg_type.typarray :: int4 as array_oid\n\
  \    from inputs\n\
  \    join pg_namespace on pg_namespace.nspname = inputs.schema_name\n\
  \    join pg_type\n\
  \      on pg_type.typname = inputs.type_name\n\
  \      and pg_type.typnamespace = pg_namespace.oid\n\
  \    where inputs.schema_name is not null\n\
  \  )\n\
  \select * from unnamespaced_results\n\
  \union\n\
  \select * from namespaced_results"

roundtrip :: SelectTypeInfo -> Hasql.Comms.Roundtrip.Roundtrip () SelectTypeInfoResult
roundtrip params =
  Hasql.Comms.Roundtrip.queryParams () sql (encodeParams params) Pq.Binary decoder

-- | Encode the two text-array parameters directly.
-- Text OID is 25; text-array OID is 1009.
encodeParams :: SelectTypeInfo -> [Maybe (Word32, ByteString, Pq.Format)]
encodeParams (SelectTypeInfo keys) =
  let (schemaNames, typeNames) = unzip (fmap CodecsVocab.QualifiedTypeName.toNameTuple (HashSet.toList keys))
      schemaArray = Binary.encodingBytes (Binary.array 25 (encodeTextArray (encodeMaybeText schemaNames)))
      typeArray = Binary.encodingBytes (Binary.array 25 (encodeTextArray (fmap (Binary.encodingArray . Binary.text_strict) typeNames)))
   in [ Just (1009, schemaArray, Pq.Binary),
        Just (1009, typeArray, Pq.Binary)
      ]
  where
    encodeTextArray elements =
      Binary.dimensionArray foldl' id elements
    encodeMaybeText =
      fmap \case
        Nothing -> Binary.nullArray
        Just text -> Binary.encodingArray (Binary.text_strict text)

decoder :: Hasql.Comms.ResultDecoder.ResultDecoder SelectTypeInfoResult
decoder =
  Hasql.Comms.ResultDecoder.foldl step HashMap.empty rowDecoder
  where
    step acc (schemaName, typeName, typeOid, arrayOid) =
      HashMap.insert (CodecsVocab.QualifiedTypeName.QualifiedTypeName schemaName typeName) (CodecsVocab.TypeInfo.TypeInfo typeOid arrayOid) acc

-- | These four columns have permanently fixed, well-known Postgres OIDs
-- (text = 25, int4 = 23), so they're decoded directly against
-- "postgresql-binary" rather than through the dynamic codec framework.
rowDecoder :: Hasql.Comms.RowDecoder.RowDecoder (Maybe Text, Text, Word32, Word32)
rowDecoder =
  (,,,)
    <$> nullableColumn textOid BinaryDecoding.text_strict
    <*> nonNullableColumn textOid BinaryDecoding.text_strict
    <*> nonNullableColumn int4Oid BinaryDecoding.int
    <*> nonNullableColumn int4Oid BinaryDecoding.int
  where
    textOid = 25
    int4Oid = 23

    nullableColumn oid valueDecoder =
      Hasql.Comms.RowDecoder.nullableColumn
        (Just oid)
        (BinaryDecoding.valueParser valueDecoder)

    nonNullableColumn oid valueDecoder =
      Hasql.Comms.RowDecoder.nonNullableColumn
        (Just oid)
        (BinaryDecoding.valueParser valueDecoder)
