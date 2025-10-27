module Hasql.Engine.PqProcedures.SelectTypeInfo
  ( SelectTypeInfo (..),
    SelectTypeInfoResult,
    run,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Hasql.Codecs.Decoders.Value qualified as Decoders.Value
import Hasql.Codecs.Encoders qualified as Encoders
import Hasql.Codecs.Encoders.Params qualified as Encoders.Params
import Hasql.Comms.ResultDecoder qualified
import Hasql.Comms.Roundtrip qualified
import Hasql.Comms.RowDecoder qualified
import Hasql.Engine.Errors qualified as Errors
import Hasql.Platform.Prelude
import Hasql.Pq qualified as Pq

newtype SelectTypeInfo = SelectTypeInfo
  { -- | Set of (schema name, type name) pairs to look up.
    keys :: HashSet (Maybe Text, Text)
  }

-- | Result maps (schema name, type name) pairs to (scalar OID, array OID) pairs.
type SelectTypeInfoResult =
  HashMap (Maybe Text, Text) (Word32, Word32)

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

encodeParams :: SelectTypeInfo -> [Maybe (Pq.Oid, ByteString, Pq.Format)]
encodeParams =
  fmap
    ( fmap
        ( \(oid, bytes, format) ->
            ( Pq.Oid (fromIntegral oid),
              bytes,
              bool Pq.Binary Pq.Text format
            )
        )
    )
    . Encoders.Params.compileUnpreparedStatementData paramsEncoder mempty

paramsEncoder :: Encoders.Params SelectTypeInfo
paramsEncoder =
  (\(SelectTypeInfo keys) -> unzip (HashSet.toList keys))
    >$< mconcat
      [ fst >$< Encoders.param (Encoders.nonNullable (Encoders.foldableArray (Encoders.nullable Encoders.text))),
        snd >$< Encoders.param (Encoders.nonNullable (Encoders.foldableArray (Encoders.nonNullable Encoders.text)))
      ]

decoder :: Hasql.Comms.ResultDecoder.ResultDecoder SelectTypeInfoResult
decoder =
  Hasql.Comms.ResultDecoder.foldl step HashMap.empty rowDecoder
  where
    step acc (schemaName, typeName, typeOid, arrayOid) =
      HashMap.insert (schemaName, typeName) (typeOid, arrayOid) acc

rowDecoder :: Hasql.Comms.RowDecoder.RowDecoder (Maybe Text, Text, Word32, Word32)
rowDecoder =
  (,,,)
    <$> nullableColumn Decoders.Value.text
    <*> nonNullableColumn Decoders.Value.text
    <*> nonNullableColumn (fromIntegral <$> Decoders.Value.int4)
    <*> nonNullableColumn (fromIntegral <$> Decoders.Value.int4)
  where
    nullableColumn valueDecoder =
      Hasql.Comms.RowDecoder.nullableColumn
        (Decoders.Value.toBaseOid valueDecoder)
        (Decoders.Value.toByteStringParser valueDecoder mempty)

    nonNullableColumn valueDecoder =
      Hasql.Comms.RowDecoder.nonNullableColumn
        (Decoders.Value.toBaseOid valueDecoder)
        (Decoders.Value.toByteStringParser valueDecoder mempty)
