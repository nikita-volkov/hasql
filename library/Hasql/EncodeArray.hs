module Hasql.EncodeArray where

import Hasql.Prelude
import qualified Hasql.EncodePrimitive as B
import qualified PostgreSQL.Binary.Encoding as A


data EncodeArray array =
  EncodeArray Word32 Word32 (array -> A.Array) (array -> A.Array)

primitive :: B.EncodePrimitive element -> EncodeArray element
primitive (B.EncodePrimitive elementOID arrayOID encoder1 encoder2) =
  EncodeArray elementOID arrayOID (A.encodingArray . encoder1) (A.encodingArray . encoder2)

nullablePrimitive :: B.EncodePrimitive element -> EncodeArray (Maybe element)
nullablePrimitive (B.EncodePrimitive elementOID arrayOID encoder1 encoder2) =
  EncodeArray elementOID arrayOID (arrayEncoder encoder1) (arrayEncoder encoder2)
  where
    arrayEncoder encoder =
      maybe A.nullArray (A.encodingArray . encoder)
