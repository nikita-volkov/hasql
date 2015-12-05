module Hasql.Encoders.Params where

import Hasql.Prelude
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified PostgreSQL.Binary.Encoder as Encoder
import qualified Hasql.Encoders.Value as Value
import qualified Hasql.PTI as PTI


-- |
-- Encoder of some representation of a parameters product.
newtype Params a =
  Params (Op (DList (LibPQ.Oid, Bool -> Maybe ByteString)) a)
  deriving (Contravariant, Divisible, Monoid)

run :: Params a -> a -> DList (LibPQ.Oid, Bool -> Maybe ByteString)
run (Params (Op op)) params =
  {-# SCC "run" #-} 
  op params

run' :: Params a -> a -> Bool -> ([LibPQ.Oid], [Maybe (ByteString, LibPQ.Format)])
run' (Params (Op op)) params integerDatetimes =
  {-# SCC "run'" #-} 
  foldr step ([], []) (op params)
  where
    step (oid, bytesGetter) ~(oidList, bytesAndFormatList) =
      (,)
        (oid : oidList)
        (fmap (\bytes -> (bytes, LibPQ.Binary)) (bytesGetter integerDatetimes) : bytesAndFormatList)

run'' :: Params a -> a -> Bool -> [Maybe (LibPQ.Oid, ByteString, LibPQ.Format)]
run'' (Params (Op op)) params integerDatetimes =
  {-# SCC "run''" #-} 
  foldr step [] (op params)
  where
    step a b =
      mapping a : b
      where
        mapping (oid, bytesGetter) =
          (,,) <$> pure oid <*> bytesGetter integerDatetimes <*> pure LibPQ.Binary

value :: Value.Value a -> Params a
value =
  contramap Just . nullableValue

nullableValue :: Value.Value a -> Params (Maybe a)
nullableValue (Value.Value valueOID arrayOID encoder') =
  Params $ Op $ \input -> 
    pure (PTI.oidPQ valueOID, \env -> fmap (Encoder.run (encoder' env)) input)
