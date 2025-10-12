module Codecs.ArrayEncoder where

import Codecs.PostgresTypeInfo qualified as B
import Platform.Prelude
import PostgreSQL.Binary.Encoding qualified as A
import TextBuilder qualified as C

data ArrayEncoder a
  = ArrayEncoder B.OID B.OID (a -> A.Array) (a -> C.TextBuilder)

instance Contravariant ArrayEncoder where
  contramap fn (ArrayEncoder valueOid arrayOid encoder renderer) =
    ArrayEncoder valueOid arrayOid (encoder . fn) (renderer . fn)

{-# INLINE value #-}
value :: B.OID -> B.OID -> (a -> A.Encoding) -> (a -> C.TextBuilder) -> ArrayEncoder a
value valueOID arrayOID encoder =
  ArrayEncoder valueOID arrayOID (A.encodingArray . encoder)

{-# INLINE nullableValue #-}
nullableValue :: B.OID -> B.OID -> (a -> A.Encoding) -> (a -> C.TextBuilder) -> ArrayEncoder (Maybe a)
nullableValue valueOID arrayOID encoder renderer =
  let maybeEncoder =
        maybe A.nullArray (A.encodingArray . encoder)
      maybeRenderer =
        maybe (C.string "null") renderer
   in ArrayEncoder valueOID arrayOID maybeEncoder maybeRenderer

{-# INLINE dimension #-}
dimension :: (forall a. (a -> b -> a) -> a -> c -> a) -> ArrayEncoder b -> ArrayEncoder c
dimension fold (ArrayEncoder valueOID arrayOID elEncoder elRenderer) =
  let encoder =
        A.dimensionArray fold elEncoder
      renderer els =
        let folded =
              let step builder el =
                    if C.isEmpty builder
                      then C.char '[' <> elRenderer el
                      else builder <> C.string ", " <> elRenderer el
               in fold step mempty els
         in if C.isEmpty folded
              then C.string "[]"
              else folded <> C.char ']'
   in ArrayEncoder valueOID arrayOID encoder renderer
