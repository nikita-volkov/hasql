module Core.Contexts.CompositeDecoder where

import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as A

newtype CompositeDecoder a
  = CompositeDecoder (ReaderT Bool A.Composite a)
  deriving (Functor, Applicative, Monad, MonadFail)

{-# INLINE run #-}
run :: CompositeDecoder a -> Bool -> A.Value a
run (CompositeDecoder imp) env =
  A.composite (runReaderT imp env)

{-# INLINE value #-}
value :: (Bool -> A.Value a) -> CompositeDecoder (Maybe a)
value decoder' =
  CompositeDecoder $ ReaderT $ A.nullableValueComposite . decoder'

{-# INLINE nonNullValue #-}
nonNullValue :: (Bool -> A.Value a) -> CompositeDecoder a
nonNullValue decoder' =
  CompositeDecoder $ ReaderT $ A.valueComposite . decoder'
