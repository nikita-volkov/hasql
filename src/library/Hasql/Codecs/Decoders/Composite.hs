module Hasql.Codecs.Decoders.Composite where

import Hasql.Codecs.Decoders.NullableOrNot qualified as NullableOrNot
import Hasql.Codecs.Decoders.Value qualified as Value
import Hasql.Codecs.RequestingOid qualified as RequestingOid
import Hasql.Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as Binary

-- |
-- Composable decoder of composite values (rows, records).
newtype Composite a
  = Composite (RequestingOid.RequestingOid Binary.Composite a)
  deriving newtype
    (Functor, Applicative)

toValueDecoder :: Composite a -> RequestingOid.RequestingOid Binary.Value a
toValueDecoder (Composite imp) =
  RequestingOid.hoist Binary.composite imp

-- |
-- Lift a 'Value.Value' decoder into a 'Composite' decoder for parsing of component values.
field :: NullableOrNot.NullableOrNot Value.Value a -> Composite a
field = \case
  NullableOrNot.NonNullable imp ->
    case Value.toBaseOid imp of
      Just oid ->
        Composite (RequestingOid.hoist (Binary.typedValueComposite oid) (Value.toDecoder imp))
      Nothing ->
        Composite
          ( RequestingOid.hoistLookingUp
              (Value.toSchema imp, Value.toTypeName imp)
              (\(baseOid, _arrayOid) -> Binary.typedValueComposite baseOid)
              (Value.toDecoder imp)
          )
  NullableOrNot.Nullable imp ->
    case Value.toBaseOid imp of
      Just oid ->
        Composite (RequestingOid.hoist (Binary.typedNullableValueComposite oid) (Value.toDecoder imp))
      Nothing ->
        Composite
          ( RequestingOid.hoistLookingUp
              (Value.toSchema imp, Value.toTypeName imp)
              (\(baseOid, _arrayOid) -> Binary.typedNullableValueComposite baseOid)
              (Value.toDecoder imp)
          )
