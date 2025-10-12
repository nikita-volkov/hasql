module Codecs.CompositeDecoder where

import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as A

newtype CompositeDecoder a
  = CompositeDecoder (A.Composite a)
  deriving newtype (Functor, Applicative, Monad, MonadFail)

{-# INLINE run #-}
run :: CompositeDecoder a -> A.Value a
run (CompositeDecoder imp) =
  A.composite imp

{-# INLINE value #-}
value :: A.Value a -> CompositeDecoder (Maybe a)
value decoder' =
  CompositeDecoder $ A.nullableValueComposite decoder'

{-# INLINE nonNullValue #-}
nonNullValue :: A.Value a -> CompositeDecoder a
nonNullValue decoder' =
  CompositeDecoder $ A.valueComposite decoder'
