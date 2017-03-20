module Hasql.Private.Encoders.Array where

import Hasql.Private.Prelude
import qualified PostgreSQL.Binary.Encoding as A
import qualified Hasql.Private.PTI as B


data Array a =
  Array B.OID B.OID (Bool -> a -> A.Array)

{-# INLINE run #-}
run :: Array a -> (B.OID, Bool -> a -> A.Encoding)
run (Array valueOID arrayOID encoder) =
  (arrayOID, \env input -> A.array (B.oidWord32 valueOID) (encoder env input))

{-# INLINE value #-}
value :: B.OID -> B.OID -> (Bool -> a -> A.Encoding) -> Array a
value valueOID arrayOID encoder =
  Array valueOID arrayOID (\params -> A.encodingArray . encoder params)

{-# INLINE nullableValue #-}
nullableValue :: B.OID -> B.OID -> (Bool -> a -> A.Encoding) -> Array (Maybe a)
nullableValue valueOID arrayOID encoder =
  Array valueOID arrayOID (\params -> maybe A.nullArray (A.encodingArray . encoder params))

{-# INLINE dimension #-}
dimension :: (forall a. (a -> b -> a) -> a -> c -> a) -> Array b -> Array c
dimension fold (Array valueOID arrayOID encoder) =
  Array valueOID arrayOID (\params -> A.dimensionArray fold (encoder params))

