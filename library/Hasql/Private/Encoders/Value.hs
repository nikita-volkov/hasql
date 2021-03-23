module Hasql.Private.Encoders.Value where

import Hasql.Private.Prelude
import qualified PostgreSQL.Binary.Encoding as B
import qualified Hasql.Private.PTI as PTI
import qualified Text.Builder as C


data Value a =
  Value PTI.OID PTI.OID (Bool -> a -> B.Encoding) (a -> C.Builder)

instance Contravariant Value where
  {-# INLINE contramap #-}
  contramap f (Value valueOID arrayOID encode render) =
    Value valueOID arrayOID (\integerDatetimes input -> encode integerDatetimes (f input)) (render . f)

{-# INLINE unsafePTI #-}
unsafePTI :: PTI.PTI -> (Bool -> a -> B.Encoding) -> (a -> C.Builder) -> Value a
unsafePTI pti =
  Value (PTI.ptiOID pti) (fromMaybe (error "No array OID") (PTI.ptiArrayOID pti))

{-# INLINE unsafePTIWithShow #-}
unsafePTIWithShow :: Show a => PTI.PTI -> (Bool -> a -> B.Encoding) -> Value a
unsafePTIWithShow pti encode =
  unsafePTI pti encode (C.string . show)
