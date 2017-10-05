module Hasql.Core.EncodedParams where

import Hasql.Prelude
import Hasql.Core.Model
import qualified ByteString.StrictBuilder as E
import qualified VectorBuilder.Builder as N


data EncodedParams =
  EncodedParams (N.Builder Word32) (Bool -> E.Builder)

instance Semigroup EncodedParams where
  (<>) (EncodedParams leftOids leftBytes) (EncodedParams rightOids rightBytes) =
    EncodedParams (leftOids <> rightOids) (leftBytes <> rightBytes)

instance Monoid EncodedParams where
  mempty = EncodedParams mempty mempty
  mappend = (<>)

param :: Word32 -> (Bool -> E.Builder) -> EncodedParams
param oid valueBuilder =
  EncodedParams (N.singleton oid) valueBuilder
