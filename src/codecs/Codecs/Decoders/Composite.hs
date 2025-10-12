module Codecs.Decoders.Composite where

import Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as Binary

newtype CompositeDecoder a
  = CompositeDecoder (Binary.Composite a)
  deriving newtype (Functor, Applicative, Monad, MonadFail)

{-# INLINE run #-}
run :: CompositeDecoder a -> Binary.Value a
run (CompositeDecoder imp) =
  Binary.composite imp

{-# INLINE value #-}
value :: Binary.Value a -> CompositeDecoder (Maybe a)
value decoder' =
  CompositeDecoder $ Binary.nullableValueComposite decoder'

{-# INLINE nonNullValue #-}
nonNullValue :: Binary.Value a -> CompositeDecoder a
nonNullValue decoder' =
  CompositeDecoder $ Binary.valueComposite decoder'
