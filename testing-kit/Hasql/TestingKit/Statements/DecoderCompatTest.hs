-- |
-- Test module to demonstrate decoder compatibility checking
--
-- This module contains test cases that would trigger the new decoder
-- compatibility errors when run against a real PostgreSQL database.
module Hasql.TestingKit.Statements.DecoderCompatTest where

import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Hasql.TestingKit.Preludes.Base

-- Test case 1: The classic wrong decoder case from WrongDecoder
-- This tries to decode generate_series (returns INT8) as UUID
wrongUuidDecoder :: Statement.Statement (Int64, Int64) [UUID]
wrongUuidDecoder = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT generate_series($1, $2)"
    encoder =
      mconcat
        [ fst >$< Encoders.param (Encoders.nonNullable Encoders.int8),
          snd >$< Encoders.param (Encoders.nonNullable Encoders.int8)
        ]
    decoder =
      Decoders.rowList
        (Decoders.column (Decoders.nonNullable Decoders.uuid))

-- Test case 2: Trying to decode int8 as bool
wrongBoolDecoder :: Statement.Statement Int64 Bool
wrongBoolDecoder = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT $1::int8"
    encoder = Encoders.param (Encoders.nonNullable Encoders.int8)
    decoder =
      Decoders.singleRow
        (Decoders.column (Decoders.nonNullable Decoders.bool))

-- Session to test the wrong UUID decoder
-- When run, this should produce a DecoderTypeMismatch error
-- indicating INT8 (OID 20) vs expected UUID (OID 2950)
testWrongUuidSession :: Session.Session [UUID]
testWrongUuidSession = Session.statement (1, 5) wrongUuidDecoder

-- Expected error message would be something like:
-- "Error in row 0, column 0: DecoderTypeMismatch 20 \"Likely type mismatch: found int8 data\""
-- Where 20 is the OID for INT8 and 2950 would be expected for UUID
