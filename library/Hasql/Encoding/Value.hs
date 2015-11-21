module Hasql.Encoding.Value where

import Hasql.Prelude
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified PostgreSQL.Binary.Encoder as Encoder
import qualified Hasql.PTI as PTI


data Value a =
  Value PTI.OID PTI.OID (Bool -> Encoder.Encoder a)

instance Contravariant Value where
  {-# INLINE contramap #-}
  contramap f (Value valueOID arrayOID encoder) =
    Value valueOID arrayOID (\integerDatetimes input -> encoder integerDatetimes (f input))

{-# INLINE run #-}
run :: Value a -> (PTI.OID, PTI.OID, Bool -> Encoder.Encoder a)
run (Value valueOID arrayOID encoder') =
  (valueOID, arrayOID, encoder')

{-# INLINE unsafePTI #-}
unsafePTI :: PTI.PTI -> (Bool -> Encoder.Encoder a) -> Value a
unsafePTI pti encoder' =
  Value (PTI.ptiOID pti) (fromMaybe ($bug "No array OID") (PTI.ptiArrayOID pti)) encoder'


