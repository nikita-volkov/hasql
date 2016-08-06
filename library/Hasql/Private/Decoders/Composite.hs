module Hasql.Private.Decoders.Composite where

import Hasql.Private.Prelude
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified PostgreSQL.Binary.Decoder as Decoder


newtype Composite a =
  Composite (ReaderT Bool Decoder.CompositeDecoder a)
  deriving (Functor, Applicative, Monad)

{-# INLINE run #-}
run :: Composite a -> Bool -> Decoder.Decoder a
run (Composite imp) env =
  Decoder.composite (runReaderT imp env)

{-# INLINE value #-}
value :: (Bool -> Decoder.Decoder a) -> Composite (Maybe a)
value decoder' =
  Composite $ ReaderT $ Decoder.compositeValue . decoder'

{-# INLINE nonNullValue #-}
nonNullValue :: (Bool -> Decoder.Decoder a) -> Composite a
nonNullValue decoder' =
  Composite $ ReaderT $ Decoder.compositeNonNullValue . decoder'

