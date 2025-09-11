-- |
-- Utilities for tracking expected column types in decoders
module Hasql.DecoderCompat where

import Hasql.LibPq14 qualified as Pq
import Hasql.Prelude

-- | Map common PostgreSQL type OIDs to their names for error messages
oidToTypeName :: Word32 -> Text
oidToTypeName oid = case oid of
  16 -> "bool"
  20 -> "int8"
  21 -> "int2"
  23 -> "int4"
  25 -> "text"
  700 -> "float4"
  701 -> "float8"
  1700 -> "numeric"
  2950 -> "uuid"
  _ -> "unknown(OID " <> fromString (show oid) <> ")"

-- | Check if an OID represents a type that could be commonly mistaken
-- This is used to provide better error messages for common decoder mismatches
isCommonlyMistaken :: Word32 -> Word32 -> Bool
isCommonlyMistaken actual expected =
  case (actual, expected) of
    -- INT8 mistaken for UUID (the WrongDecoder test case)
    (20, 2950) -> True
    -- UUID mistaken for INT8
    (2950, 20) -> True
    -- Other common mismatches could be added here
    _ -> False
