module Hasql.Encoders.Params where

import qualified Database.PostgreSQL.LibPQ as A
import qualified Hasql.Encoders.Value as C
import qualified Hasql.PostgresTypeInfo as D
import Hasql.Prelude
import qualified PostgreSQL.Binary.Encoding as B
import qualified Text.Builder as E

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
