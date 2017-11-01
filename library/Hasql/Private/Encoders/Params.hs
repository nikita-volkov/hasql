module Hasql.Private.Encoders.Params where

import Hasql.Private.Prelude
import qualified Database.PostgreSQL.LibPQ as A
import qualified PostgreSQL.Binary.Encoding as B
import qualified Hasql.Private.Encoders.Value as C
import qualified Hasql.Private.PTI as D


-- |
-- Encoder of some representation of a parameters product.
newtype Params a =
  Params (Op (DList (A.Oid, Bool -> Maybe ByteString)) a)
  deriving (Contravariant, Divisible, Decidable, Monoid)

instance Semigroup (Params a)

run :: Params a -> a -> DList (A.Oid, Bool -> Maybe ByteString)
run (Params (Op op)) params =
  {-# SCC "run" #-} 
  op params

run' :: Params a -> a -> Bool -> ([A.Oid], [Maybe (ByteString, A.Format)])
run' (Params (Op op)) params integerDatetimes =
  {-# SCC "run'" #-} 
  foldr step ([], []) (op params)
  where
    step (oid, bytesGetter) ~(oidList, bytesAndFormatList) =
      (,)
        (oid : oidList)
        (fmap (\bytes -> (bytes, format oid)) (bytesGetter integerDatetimes) : bytesAndFormatList)

run'' :: Params a -> a -> Bool -> [Maybe (A.Oid, ByteString, A.Format)]
run'' (Params (Op op)) params integerDatetimes =
  {-# SCC "run''" #-} 
  foldr step [] (op params)
  where
    step a b =
      mapping a : b
      where
        mapping (oid, bytesGetter) =
          (,,) <$> pure oid <*> bytesGetter integerDatetimes <*> pure (format oid)

format :: A.Oid -> A.Format
format oid = case oid of
  A.Oid 705 -> A.Text -- is unknown
  _         -> A.Binary

value :: C.Value a -> Params a
value =
  contramap Just . nullableValue

nullableValue :: C.Value a -> Params (Maybe a)
nullableValue (C.Value valueOID arrayOID encoder) =
  Params $ Op $ \input -> 
    pure (D.oidPQ valueOID, \env -> fmap (B.encodingBytes . encoder env) input)
