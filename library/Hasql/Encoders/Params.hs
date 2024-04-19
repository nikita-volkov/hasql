module Hasql.Encoders.Params where

import Database.PostgreSQL.LibPQ qualified as A
import Hasql.Encoders.Value qualified as C
import Hasql.PostgresTypeInfo qualified as D
import Hasql.Prelude
import PostgreSQL.Binary.Encoding qualified as B
import Text.Builder qualified as E

-- |
-- Encoder of some representation of a parameters product.
newtype Params a
  = Params (Op (DList (A.Oid, A.Format, Bool -> Maybe ByteString, Text)) a)
  deriving (Contravariant, Divisible, Decidable, Semigroup, Monoid)

value :: C.Value a -> Params a
value =
  contramap Just . nullableValue

nullableValue :: C.Value a -> Params (Maybe a)
nullableValue (C.Value valueOID arrayOID encode render) =
  Params
    $ Op
    $ \input ->
      let D.OID _ pqOid format =
            valueOID
          encoder env =
            fmap (B.encodingBytes . encode env) input
          rendering =
            maybe "null" (E.run . render) input
       in pure (pqOid, format, encoder, rendering)
