module Hasql.Decoders.Array where

import Hasql.Prelude
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified PostgreSQL.Binary.Decoder as Decoder


newtype Array a =
  Array (ReaderT Bool Decoder.ArrayDecoder a)
  deriving (Functor)

{-# INLINE run #-}
run :: Array a -> Bool -> Decoder.Decoder a
run (Array imp) env =
  Decoder.array (runReaderT imp env)

{-# INLINE dimension #-}
dimension :: (forall m. Monad m => Int -> m a -> m b) -> Array a -> Array b
dimension replicateM (Array imp) =
  Array $ ReaderT $ \env -> Decoder.arrayDimension replicateM (runReaderT imp env)

{-# INLINE value #-}
value :: (Bool -> Decoder.Decoder a) -> Array (Maybe a)
value decoder' =
  Array $ ReaderT $ Decoder.arrayValue . decoder'

{-# INLINE nonNullValue #-}
nonNullValue :: (Bool -> Decoder.Decoder a) -> Array a
nonNullValue decoder' =
  Array $ ReaderT $ Decoder.arrayNonNullValue . decoder'

