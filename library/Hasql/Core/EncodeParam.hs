module Hasql.Core.EncodeParam where

import Hasql.Prelude
import Hasql.Core.Model
import qualified ByteString.StrictBuilder as B
import qualified VectorBuilder.Builder as N
import qualified PostgreSQL.Binary.Encoding as A
import qualified Hasql.Core.EncodePrimitive as C


data EncodeParam param =
  EncodeParam Word32 (param -> B.Builder) (param -> B.Builder)

instance Contravariant EncodeParam where
  contramap mapping (EncodeParam oid idtOnEncode idtOffEncode) =
    EncodeParam oid (idtOnEncode . mapping) (idtOffEncode . mapping)

primitive :: C.EncodePrimitive primitive -> EncodeParam primitive
primitive (C.EncodePrimitive elementOID _ builder1 builder2) =
  EncodeParam elementOID builder1 builder2

arrayVector :: C.EncodePrimitive primitive -> EncodeParam (Vector primitive)
arrayVector (C.EncodePrimitive elementOID arrayOID elementEncoder1 elementEncoder2) =
  EncodeParam arrayOID (encoder elementEncoder1) (encoder elementEncoder2)
  where
    encoder elementEncoder =
      A.array_vector elementOID elementEncoder

arrayVectorWithNulls :: C.EncodePrimitive primitive -> EncodeParam (Vector (Maybe primitive))
arrayVectorWithNulls (C.EncodePrimitive elementOID arrayOID elementEncoder1 elementEncoder2) =
  EncodeParam arrayOID (encoder elementEncoder1) (encoder elementEncoder2)
  where
    encoder elementEncoder =
      A.nullableArray_vector elementOID elementEncoder
