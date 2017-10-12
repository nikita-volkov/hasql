module Hasql.EncodePrimitive where

import Hasql.Prelude
import Hasql.Model
import qualified ByteString.StrictBuilder as B
import qualified PostgreSQL.Binary.Encoding as A
import qualified PostgreSQL.Binary.Data as F
import qualified Hasql.OID.Primitive as D
import qualified Hasql.OID.Array as E


data EncodePrimitive primitive =
  EncodePrimitive Word32 Word32 (primitive -> B.Builder) (primitive -> B.Builder)

instance Contravariant EncodePrimitive where
  {-# INLINE contramap #-}
  contramap fn (EncodePrimitive primitiveOID arrayOID builder1 builder2) =
    EncodePrimitive primitiveOID arrayOID (builder1 . fn) (builder2 . fn)

int2 :: EncodePrimitive Int16
int2 =
  EncodePrimitive D.int2 E.int2 A.int2_int16 A.int2_int16

int4 :: EncodePrimitive Int32
int4 =
  EncodePrimitive D.int4 E.int4 A.int4_int32 A.int4_int32

int8 :: EncodePrimitive Int64
int8 =
  EncodePrimitive D.int8 E.int8 A.int8_int64 A.int8_int64

text :: EncodePrimitive Text
text =
  EncodePrimitive D.text E.text A.text_strict A.text_strict


-- ** Time
-------------------------

date :: EncodePrimitive F.Day
date =
  EncodePrimitive D.date E.date A.date A.date

time :: EncodePrimitive F.TimeOfDay
time =
  EncodePrimitive D.time E.time A.time_int A.time_float

timetz :: EncodePrimitive (F.TimeOfDay, F.TimeZone)
timetz =
  EncodePrimitive D.timetz E.timetz A.timetz_int A.timetz_float

timestamp :: EncodePrimitive F.LocalTime
timestamp =
  EncodePrimitive D.timestamp E.timestamp A.timestamp_int A.timestamp_float

timestamptz :: EncodePrimitive F.UTCTime
timestamptz =
  EncodePrimitive D.timestamptz E.timestamptz A.timestamptz_int A.timestamptz_float

interval :: EncodePrimitive F.DiffTime
interval =
  EncodePrimitive D.interval E.interval A.interval_int A.interval_float
