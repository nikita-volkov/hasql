module Hasql.Encoders.Value where

import Hasql.PostgresTypeInfo qualified as PTI
import Hasql.Prelude
import PostgreSQL.Binary.Encoding qualified as B
import TextBuilder qualified as C

data Value a
  = Value
      -- | Type name.
      Text
      -- | Statically known OID for the type.
      -- When unspecified, the OID may be determined at runtime by looking up by name.
      (Maybe PTI.OID)
      -- | Statically known OID for the array-type with this type as the element.
      -- When unspecified, the OID may be determined at runtime by looking up by name.
      -- It may also mean that there may be no array type containing this type, which is the case in attempts to double-nest arrays.
      (Maybe PTI.OID)
      -- | Serialization function.
      (Bool -> a -> B.Encoding)
      -- | Render function for error messages.
      (a -> C.TextBuilder)

instance Contravariant Value where
  {-# INLINE contramap #-}
  contramap f (Value typeName valueOID arrayOID encode render) =
    Value typeName valueOID arrayOID (\integerDatetimes input -> encode integerDatetimes (f input)) (render . f)

{-# INLINE unsafePTI #-}
unsafePTI :: PTI.PTI -> (Bool -> a -> B.Encoding) -> (a -> C.TextBuilder) -> Value a
unsafePTI pti =
  Value "unknown" (Just (PTI.ptiOID pti)) (PTI.ptiArrayOID pti)

{-# INLINE unsafePTIWithName #-}
unsafePTIWithName :: Text -> PTI.PTI -> (Bool -> a -> B.Encoding) -> (a -> C.TextBuilder) -> Value a
unsafePTIWithName typeName pti =
  Value typeName (Just (PTI.ptiOID pti)) (PTI.ptiArrayOID pti)

{-# INLINE unsafePTIWithShow #-}
unsafePTIWithShow :: (Show a) => PTI.PTI -> (Bool -> a -> B.Encoding) -> Value a
unsafePTIWithShow pti encode =
  unsafePTI pti encode (C.string . show)
