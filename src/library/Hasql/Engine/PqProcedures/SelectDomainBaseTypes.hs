module Hasql.Engine.PqProcedures.SelectDomainBaseTypes
  ( run,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Hasql.Codecs.Decoders.Value qualified as Decoders.Value
import Hasql.Comms.ResultDecoder qualified
import Hasql.Comms.Roundtrip qualified
import Hasql.Comms.RowDecoder qualified
import Hasql.Engine.Errors qualified as Errors
import Hasql.Platform.Prelude
import Hasql.Pq qualified as Pq

-- | Query all domain types and their base type OIDs.
-- Returns a mapping from domain OID to base type OID.
run :: Pq.Connection -> IO (Either Errors.SessionError (HashMap Word32 Word32))
run connection =
  first Errors.fromRoundtripError
    <$> Hasql.Comms.Roundtrip.toSerialIO roundtrip connection

sql :: ByteString
sql =
  "select oid :: int4, typbasetype :: int4 from pg_type where typtype = 'd'"

roundtrip :: Hasql.Comms.Roundtrip.Roundtrip () (HashMap Word32 Word32)
roundtrip =
  Hasql.Comms.Roundtrip.queryParams () sql [] Pq.Binary decoder

decoder :: Hasql.Comms.ResultDecoder.ResultDecoder (HashMap Word32 Word32)
decoder =
  Hasql.Comms.ResultDecoder.foldl HashMap.empty step HashMap.empty rowDecoder
  where
    step acc (domainOid, baseTypeOid) =
      HashMap.insert domainOid baseTypeOid acc

rowDecoder :: Hasql.Comms.RowDecoder.RowDecoder (Word32, Word32)
rowDecoder =
  (,)
    <$> nonNullableColumn (fromIntegral <$> Decoders.Value.int4)
    <*> nonNullableColumn (fromIntegral <$> Decoders.Value.int4)
  where
    nonNullableColumn valueDecoder =
      Hasql.Comms.RowDecoder.nonNullableColumn
        (Decoders.Value.toBaseOid valueDecoder)
        (Decoders.Value.toByteStringParser valueDecoder mempty)
