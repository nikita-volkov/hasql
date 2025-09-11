module Hasql.Contexts.ArrayDecoder where

import Hasql.Prelude
import PostgreSQL.Binary.Decoding qualified as A

newtype ArrayDecoder a
  = ArrayDecoder (ReaderT Bool A.Array a)
  deriving (Functor)

{-# INLINE run #-}
run :: ArrayDecoder a -> Bool -> A.Value a
run (ArrayDecoder imp) env =
  A.array (runReaderT imp env)

{-# INLINE dimension #-}
dimension :: (forall m. (Monad m) => Int -> m a -> m b) -> ArrayDecoder a -> ArrayDecoder b
dimension replicateM (ArrayDecoder imp) =
  ArrayDecoder $ ReaderT $ \env -> A.dimensionArray replicateM (runReaderT imp env)

{-# INLINE value #-}
value :: (Bool -> A.Value a) -> ArrayDecoder (Maybe a)
value decoder' =
  ArrayDecoder $ ReaderT $ A.nullableValueArray . decoder'

{-# INLINE nonNullValue #-}
nonNullValue :: (Bool -> A.Value a) -> ArrayDecoder a
nonNullValue decoder' =
  ArrayDecoder $ ReaderT $ A.valueArray . decoder'
