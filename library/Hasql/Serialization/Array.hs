module Hasql.Serialization.Array where

import Hasql.Prelude
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified PostgreSQL.Binary.Encoder as Encoder
import qualified Hasql.PTI as PTI


data Array a =
  Array PTI.OID PTI.OID (Bool -> Encoder.ArrayEncoder a)

{-# INLINE run #-}
run :: Array a -> (PTI.OID, Bool -> Encoder.Encoder a)
run (Array valueOID arrayOID encoder') =
  (arrayOID, \env -> Encoder.array (PTI.oidWord32 valueOID) (encoder' env))

{-# INLINE value #-}
value :: PTI.OID -> PTI.OID -> (Bool -> Encoder.Encoder a) -> Array a
value valueOID arrayOID encoder' =
  Array valueOID arrayOID (Encoder.arrayValue . encoder')

{-# INLINE nullableValue #-}
nullableValue :: PTI.OID -> PTI.OID -> (Bool -> Encoder.Encoder a) -> Array (Maybe a)
nullableValue valueOID arrayOID encoder' =
  Array valueOID arrayOID (Encoder.arrayNullableValue . encoder')

{-# INLINE dimension #-}
dimension :: (forall a. (a -> b -> a) -> a -> c -> a) -> Array b -> Array c
dimension foldl (Array valueOID arrayOID encoder') =
  Array valueOID arrayOID (Encoder.arrayDimension foldl . encoder')

