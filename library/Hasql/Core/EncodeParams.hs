module Hasql.Core.EncodeParams where

import Hasql.Prelude
import Hasql.Core.Model
import qualified ByteString.StrictBuilder as B
import qualified VectorBuilder.Builder as N
import qualified Hasql.Core.EncodeParam as A
import qualified Hasql.Core.Protocol.Encoding as C


data EncodeParams input =
  EncodeParams (N.Builder Word32) (input -> B.Builder) (input -> B.Builder)

instance Semigroup (EncodeParams input) where
  {-# INLINE (<>) #-}
  (<>) (EncodeParams leftOids leftBuilder1 leftBuilder2) (EncodeParams rightOids rightBuilder1 rightBuilder2) =
    EncodeParams oids builder1 builder2
    where
      oids =
        leftOids <> rightOids
      builder1 =
        leftBuilder1 <> rightBuilder1
      builder2 =
        leftBuilder2 <> rightBuilder2

instance Monoid (EncodeParams input) where
  {-# INLINE mempty #-}
  mempty =
    EncodeParams mempty mempty mempty
  {-# INLINE mappend #-}
  mappend =
    (<>)

instance Contravariant EncodeParams where
  {-# INLINE contramap #-}
  contramap fn (EncodeParams oids builder1 builder2) =
    EncodeParams oids (builder1 . fn) (builder2 . fn)

instance Divisible EncodeParams where
  {-# INLINE conquer #-}
  conquer =
    mempty
  {-# INLINABLE divide #-}
  divide fn (EncodeParams leftOids leftBuilder1 leftBuilder2) (EncodeParams rightOids rightBuilder1 rightBuilder2) =
    EncodeParams oids builder1 builder2
    where
      oids =
        leftOids <> rightOids
      builder1 =
        mergedBuilder leftBuilder1 rightBuilder1
      builder2 =
        mergedBuilder leftBuilder2 rightBuilder2
      mergedBuilder leftBuilder rightBuilder params =
        case fn params of
          (leftParams, rightParams) ->
            leftBuilder leftParams <>
            rightBuilder rightParams

param :: A.EncodeParam param -> EncodeParams param
param (A.EncodeParam oid idtOnEncode idtOffEncode) =
  EncodeParams (N.singleton oid) (C.sizedValue . idtOnEncode) (C.sizedValue . idtOffEncode)

nullableParam :: A.EncodeParam param -> EncodeParams (Maybe param)
nullableParam (A.EncodeParam oid idtOnEncode idtOffEncode) =
  EncodeParams (N.singleton oid) (C.nullableSizedValue . fmap idtOnEncode) (C.nullableSizedValue . fmap idtOffEncode)
