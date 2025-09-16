module Core.Contexts.OIDLookup where

import Core.Contexts.Command qualified as Command
import Core.Contexts.ResultConsumer qualified as ResultConsumer
import Core.Contexts.RowDecoder qualified as RowDecoder  
import Core.Contexts.ValueDecoder qualified as ValueDecoder
import Core.PostgresTypeInfo qualified as PTI
import Core.Structures.OIDCache qualified as OIDCache
import Data.Text.Encoding qualified as Text
import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as A
import Pq qualified

-- | Look up type information by name from pg_type
lookupTypeInfo :: Text -> Command.Command (Maybe OIDCache.TypeInfo)
lookupTypeInfo typeName = do
  let sql = "SELECT t.oid, COALESCE(a.oid, 0) FROM pg_type t LEFT JOIN pg_type a ON a.typname = t.typname || '_array' WHERE t.typname = $1"
  Command.sendQueryParams sql [(Just (Pq.Oid 25, Text.encodeUtf8 typeName, Pq.Binary))]
  result <- Command.consumeResult (ResultConsumer.maybe rowDecoder)
  Command.drainResults
  pure result
  where
    rowDecoder = do
      typeOidInt <- RowDecoder.nonNullableColumn int4Decoder
      arrayOidInt <- RowDecoder.nonNullableColumn int4Decoder
      let typeOid = Pq.Oid (fromIntegral typeOidInt)
      let arrayOid = if arrayOidInt == 0 then Nothing else Just (Pq.Oid (fromIntegral arrayOidInt))
      pure (OIDCache.TypeInfo typeOid arrayOid)

    int4Decoder = ValueDecoder.ValueDecoder "int4" (Just (PTI.ptiOID PTI.int4)) Nothing A.int