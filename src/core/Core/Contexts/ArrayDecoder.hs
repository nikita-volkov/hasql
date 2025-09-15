module Core.Contexts.ArrayDecoder where

import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as A

newtype ArrayDecoder a
  = ArrayDecoder (A.Array a)
  deriving (Functor)

{-# INLINE run #-}
run :: ArrayDecoder a -> A.Value a
run (ArrayDecoder imp) =
  A.array imp

{-# INLINE dimension #-}
dimension :: (forall m. (Monad m) => Int -> m a -> m b) -> ArrayDecoder a -> ArrayDecoder b
dimension replicateM (ArrayDecoder imp) =
  ArrayDecoder $ A.dimensionArray replicateM imp

{-# INLINE value #-}
value :: A.Value a -> ArrayDecoder (Maybe a)
value decoder' =
  ArrayDecoder $ A.nullableValueArray decoder'

{-# INLINE nonNullValue #-}
nonNullValue :: A.Value a -> ArrayDecoder a
nonNullValue decoder' =
  ArrayDecoder $ A.valueArray decoder'
