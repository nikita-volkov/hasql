module Codecs.ValueEncoder where

import Codecs.PostgresTypeInfo qualified as PTI
import Platform.Prelude
import PostgreSQL.Binary.Encoding qualified as B
import TextBuilder qualified as C

data ValueEncoder a
  = ValueEncoder
      -- | Type name.
      Text
      -- | Statically known OID for the type.
      -- When unspecified, the OID may be determined at runtime by looking up by name.
      (Maybe PTI.OID)
      -- | Statically known OID for the array-type with this type as the element.
      -- When unspecified, the OID may be determined at runtime by looking up by name.
      -- It may also mean that there may be no array type containing this type, which is the case in attempts to double-nest arrays.
      (Maybe PTI.OID)
      -- | Serialization function (always integer timestamps for PostgreSQL 10+).
      (a -> B.Encoding)
      -- | Render function for error messages.
      (a -> C.TextBuilder)

instance Contravariant ValueEncoder where
  {-# INLINE contramap #-}
  contramap f (ValueEncoder typeName valueOID arrayOID encode render) =
    ValueEncoder typeName valueOID arrayOID (encode . f) (render . f)

{-# INLINE unsafePTI #-}
unsafePTI :: PTI.PTI -> (a -> B.Encoding) -> (a -> C.TextBuilder) -> ValueEncoder a
unsafePTI pti =
  ValueEncoder "unknown" (Just (PTI.ptiOID pti)) (PTI.ptiArrayOID pti)

{-# INLINE unsafePTIWithName #-}
unsafePTIWithName :: Text -> PTI.PTI -> (a -> B.Encoding) -> (a -> C.TextBuilder) -> ValueEncoder a
unsafePTIWithName typeName pti =
  ValueEncoder typeName (Just (PTI.ptiOID pti)) (PTI.ptiArrayOID pti)

{-# INLINE unsafePTIWithShow #-}
unsafePTIWithShow :: (Show a) => PTI.PTI -> (a -> B.Encoding) -> ValueEncoder a
unsafePTIWithShow pti encode =
  unsafePTI pti encode (C.string . show)
