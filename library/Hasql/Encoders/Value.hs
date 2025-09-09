module Hasql.Encoders.Value where

import Hasql.PostgresTypeInfo qualified as PTI
import Hasql.Prelude
import PostgreSQL.Binary.Encoding qualified as B
import TextBuilder qualified as C

data Value a
  = Value
      -- | Statically known OID for the type.
      PTI.OID
      -- | Statically known OID for the array-type with this type as the element.
      PTI.OID
      -- | Serialization function.
      (Bool -> a -> B.Encoding)
      -- | Render function for error messages.
      (a -> C.TextBuilder)

instance Contravariant Value where
  {-# INLINE contramap #-}
  contramap f (Value valueOID arrayOID encode render) =
    Value valueOID arrayOID (\integerDatetimes input -> encode integerDatetimes (f input)) (render . f)

{-# INLINE unsafePTI #-}
unsafePTI :: PTI.PTI -> (Bool -> a -> B.Encoding) -> (a -> C.TextBuilder) -> Value a
unsafePTI pti =
  Value (PTI.ptiOID pti) (fromMaybe (error "No array OID") (PTI.ptiArrayOID pti))

{-# INLINE unsafePTIWithShow #-}
unsafePTIWithShow :: (Show a) => PTI.PTI -> (Bool -> a -> B.Encoding) -> Value a
unsafePTIWithShow pti encode =
  unsafePTI pti encode (C.string . show)
