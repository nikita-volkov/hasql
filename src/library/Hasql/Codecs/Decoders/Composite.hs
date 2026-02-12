module Hasql.Codecs.Decoders.Composite where

import Hasql.Codecs.Decoders.NullableOrNot qualified as NullableOrNot
import Hasql.Codecs.Decoders.Value qualified as Value
import Hasql.Codecs.RequestingOid qualified as RequestingOid
import Hasql.Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as Binary

-- |
-- Composable decoder of composite values (rows, records).
newtype Composite a
  = Composite (RequestingOid.RequestingOid (Binary.Composite a))

instance Functor Composite where
  fmap f (Composite r) = Composite (fmap (fmap f) r)

instance Applicative Composite where
  pure a = Composite (pure (pure a))
  Composite f <*> Composite x = Composite (liftA2 (<*>) f x)

toValueDecoder :: Composite a -> RequestingOid.RequestingOid (Binary.Value a)
toValueDecoder (Composite imp) =
  fmap Binary.composite imp

-- |
-- Lift a 'Value.Value' decoder into a 'Composite' decoder for parsing of component values.
field :: NullableOrNot.NullableOrNot Value.Value a -> Composite a
field = \case
  NullableOrNot.NonNullable imp ->
    let dimensionality = Value.toDimensionality imp
        staticOid = if dimensionality == 0 then Value.toBaseOid imp else Value.toArrayOid imp
     in case staticOid of
          Just oid ->
            Composite (fmap (Binary.typedValueComposite oid) (Value.toDecoder imp))
          Nothing ->
            Composite
              ( RequestingOid.hoistLookingUp
                  (Value.toSchema imp, Value.toTypeName imp)
                  (\(baseOid, arrayOid) decoder -> Binary.typedValueComposite (if dimensionality == 0 then baseOid else arrayOid) decoder)
                  (Value.toDecoder imp)
              )
  NullableOrNot.Nullable imp ->
    let dimensionality = Value.toDimensionality imp
        staticOid = if dimensionality == 0 then Value.toBaseOid imp else Value.toArrayOid imp
     in case staticOid of
          Just oid ->
            Composite (fmap (Binary.typedNullableValueComposite oid) (Value.toDecoder imp))
          Nothing ->
            Composite
              ( RequestingOid.hoistLookingUp
                  (Value.toSchema imp, Value.toTypeName imp)
                  (\(baseOid, arrayOid) decoder -> Binary.typedNullableValueComposite (if dimensionality == 0 then baseOid else arrayOid) decoder)
                  (Value.toDecoder imp)
              )
