module Hasql.Private.Encoders.Value where

import Hasql.Private.Prelude
import qualified PostgreSQL.Binary.Encoding as B
import qualified Hasql.Private.PTI as PTI


data Value a =
  Value PTI.OID PTI.OID (Bool -> a -> B.Encoding)

instance Contravariant Value where
  {-# INLINE contramap #-}
  contramap f (Value valueOID arrayOID encoder) =
    Value valueOID arrayOID (\integerDatetimes input -> encoder integerDatetimes (f input))

{-# INLINE run #-}
run :: Value a -> (PTI.OID, PTI.OID, Bool -> a -> B.Encoding)
run (Value valueOID arrayOID encoder') =
  (valueOID, arrayOID, encoder')

{-# INLINE unsafePTI #-}
unsafePTI :: PTI.PTI -> (Bool -> a -> B.Encoding) -> Value a
unsafePTI pti encoder' =
  Value (PTI.ptiOID pti) (fromMaybe ($bug "No array OID") (PTI.ptiArrayOID pti)) encoder'


