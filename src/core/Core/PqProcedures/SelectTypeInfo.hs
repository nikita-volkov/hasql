module Core.PqProcedures.SelectTypeInfo
  ( SelectTypeInfo (..),
    SelectTypeInfoResult,
    run,
  )
where

import Codecs.Decoders.Value qualified as Decoders.Value
import Codecs.Encoders qualified as Encoders
import Codecs.Encoders.Params qualified as Encoders.Params
import Core.Errors qualified as Errors
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Hipq.ResultDecoder qualified
import Hipq.Roundtrip qualified
import Hipq.RowDecoder qualified
import Platform.Prelude
import Pq qualified

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
        <$> Hipq.Roundtrip.toSerialIO (roundtrip (SelectTypeInfo keys)) connection

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
  \      inputs.type_name :: text as type_name,\n\
  \      case\n\
  \        when inputs.type_name like '%[]' then\n\
  \          coalesce(\n\
  \            (select typarray from pg_type where oid = to_regtype(substring(inputs.type_name from 1 for length(inputs.type_name) - 2))),\n\
  \            (select typarray from pg_type where typname = substring(inputs.type_name from 1 for length(inputs.type_name) - 2) order by oid limit 1)\n\
  \          )\n\
  \        else\n\
  \          coalesce(\n\
  \            (select oid from pg_type where oid = to_regtype(inputs.type_name)),\n\
  \            (select oid from pg_type where typname = inputs.type_name order by oid limit 1)\n\
  \          )\n\
  \      end :: int4 as type_oid,\n\
  \      case\n\
  \        when inputs.type_name like '%[]' then\n\
  \          coalesce(\n\
  \            (select t2.typarray from pg_type t1 join pg_type t2 on t2.oid = t1.typarray where t1.oid = to_regtype(substring(inputs.type_name from 1 for length(inputs.type_name) - 2))),\n\
  \            (select t2.typarray from pg_type t1 join pg_type t2 on t2.oid = t1.typarray where t1.typname = substring(inputs.type_name from 1 for length(inputs.type_name) - 2) order by t1.oid limit 1),\n\
  \            0\n\
  \          )\n\
  \        else\n\
  \          coalesce(\n\
  \            (select typarray from pg_type where oid = to_regtype(inputs.type_name)),\n\
  \            (select typarray from pg_type where typname = inputs.type_name order by oid limit 1),\n\
  \            0\n\
  \          )\n\
  \      end :: int4 as array_oid\n\
  \    from inputs\n\
  \    where inputs.schema_name is null\n\
  \  ),\n\
  \  namespaced_results as (\n\
  \    select\n\
  \      pg_namespace.nspname :: text as schema_name,\n\
  \      inputs.type_name :: text as type_name,\n\
  \      case\n\
  \        when inputs.type_name like '%[]' then\n\
  \          (select typarray from pg_type\n\
  \           where typname = substring(inputs.type_name from 1 for length(inputs.type_name) - 2)\n\
  \             and typnamespace = pg_namespace.oid)\n\
  \        else\n\
  \          pg_type.oid\n\
  \      end :: int4 as type_oid,\n\
  \      case\n\
  \        when inputs.type_name like '%[]' then\n\
  \          coalesce(\n\
  \            (select t2.typarray from pg_type t1\n\
  \             join pg_type t2 on t2.oid = t1.typarray\n\
  \             where t1.typname = substring(inputs.type_name from 1 for length(inputs.type_name) - 2)\n\
  \               and t1.typnamespace = pg_namespace.oid),\n\
  \            0\n\
  \          )\n\
  \        else\n\
  \          pg_type.typarray\n\
  \      end :: int4 as array_oid\n\
  \    from inputs\n\
  \    join pg_namespace on pg_namespace.nspname = inputs.schema_name\n\
  \    left join pg_type\n\
  \      on pg_type.typname = case\n\
  \          when inputs.type_name like '%[]' then substring(inputs.type_name from 1 for length(inputs.type_name) - 2)\n\
  \          else inputs.type_name\n\
  \        end\n\
  \      and pg_type.typnamespace = pg_namespace.oid\n\
  \    where inputs.schema_name is not null\n\
  \  )\n\
  \select * from unnamespaced_results\n\
  \union\n\
  \select * from namespaced_results"

roundtrip :: SelectTypeInfo -> Hipq.Roundtrip.Roundtrip () SelectTypeInfoResult
roundtrip params =
  Hipq.Roundtrip.queryParams () sql (encodeParams params) Pq.Binary decoder

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

decoder :: Hipq.ResultDecoder.ResultDecoder SelectTypeInfoResult
decoder =
  Hipq.ResultDecoder.foldl step HashMap.empty rowDecoder
  where
    step acc (schemaName, typeName, typeOid, arrayOid) =
      HashMap.insert (schemaName, typeName) (typeOid, arrayOid) acc

rowDecoder :: Hipq.RowDecoder.RowDecoder (Maybe Text, Text, Word32, Word32)
rowDecoder =
  (,,,)
    <$> nullableColumn Decoders.Value.text
    <*> nonNullableColumn Decoders.Value.text
    <*> nonNullableColumn (fromIntegral <$> Decoders.Value.int4)
    <*> nonNullableColumn (fromIntegral <$> Decoders.Value.int4)
  where
    nullableColumn valueDecoder =
      Hipq.RowDecoder.nullableColumn
        (Decoders.Value.toBaseOid valueDecoder)
        (Decoders.Value.toByteStringParser valueDecoder mempty)

    nonNullableColumn valueDecoder =
      Hipq.RowDecoder.nonNullableColumn
        (Decoders.Value.toBaseOid valueDecoder)
        (Decoders.Value.toByteStringParser valueDecoder mempty)
