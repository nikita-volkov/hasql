module Hasql.Connection.Session.Statement.Encoding.Encoding
where

import Hasql.Prelude
import qualified PostgreSQL.Binary.Data as F
import qualified PostgreSQL.Binary.Encoding as A
import qualified Hasql.OID.Primitive as D
import qualified Hasql.OID.Array as E
import qualified ByteString.StrictBuilder as B
import qualified Data.IntMap.Strict as G


-- * Param
-------------------------

data Param param =
  Param Word32 (param -> B.Builder) (param -> B.Builder)

vectorArray :: Primitive a -> Param (Vector a)
vectorArray (Primitive elementOID arrayOID elementEncoder1 elementEncoder2) =
  Param arrayOID (encoder elementEncoder1) (encoder elementEncoder2)
  where
    encoder elementEncoder =
      A.array_vector elementOID elementEncoder

vectorArrayWithNulls :: Primitive a -> Param (Vector (Maybe a))
vectorArrayWithNulls (Primitive elementOID arrayOID elementEncoder1 elementEncoder2) =
  Param arrayOID (encoder elementEncoder1) (encoder elementEncoder2)
  where
    encoder elementEncoder =
      A.nullableArray_vector elementOID elementEncoder

primitive :: Primitive primitive -> Param primitive
primitive (Primitive elementOID arrayOID builder1 builder2) =
  Param elementOID builder1 builder2


-- * Primitive
-------------------------

data Primitive primitive =
  Primitive Word32 Word32 (primitive -> B.Builder) (primitive -> B.Builder)

instance Contravariant Primitive where
  {-# INLINE contramap #-}
  contramap fn (Primitive primitiveOID arrayOID builder1 builder2) =
    Primitive primitiveOID arrayOID (builder1 . fn) (builder2 . fn)

int2 :: Primitive Int16
int2 =
  Primitive D.int2 E.int2 A.int2_int16 A.int2_int16

int4 :: Primitive Int32
int4 =
  Primitive D.int4 E.int4 A.int4_int32 A.int4_int32

int8 :: Primitive Int64
int8 =
  Primitive D.int8 E.int8 A.int8_int64 A.int8_int64

text :: Primitive Text
text =
  Primitive D.text E.text A.text_strict A.text_strict


-- ** Time
-------------------------

date :: Primitive F.Day
date =
  Primitive D.date E.date A.date A.date

time :: Primitive F.TimeOfDay
time =
  Primitive D.time E.time A.time_float A.time_int

timetz :: Primitive (F.TimeOfDay, F.TimeZone)
timetz =
  Primitive D.timetz E.timetz A.timetz_float A.timetz_int

timestamp :: Primitive F.LocalTime
timestamp =
  Primitive D.timestamp E.timestamp A.timestamp_float A.timestamp_int

timestamptz :: Primitive F.UTCTime
timestamptz =
  Primitive D.timestamptz E.timestamptz A.timestamptz_float A.timestamptz_int

interval :: Primitive F.DiffTime
interval =
  Primitive D.interval E.interval A.interval_float A.interval_int


-- * Array
-------------------------

data Array array =
  Array Word32 Word32 (array -> A.Array) (array -> A.Array)

element :: Primitive element -> Array element
element (Primitive elementOID arrayOID encoder1 encoder2) =
  Array elementOID arrayOID (A.encodingArray . encoder1) (A.encodingArray . encoder2)

nullableElement :: Primitive element -> Array (Maybe element)
nullableElement (Primitive elementOID arrayOID encoder1 encoder2) =
  Array elementOID arrayOID (arrayEncoder encoder1) (arrayEncoder encoder2)
  where
    arrayEncoder encoder =
      maybe A.nullArray (A.encodingArray . encoder)
