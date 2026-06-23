module Hasql.Codecs.Decoders.Composite where

import Hasql.Codecs.Decoders.NullableOrNot qualified as NullableOrNot
import Hasql.Codecs.Decoders.Value qualified as Value
import Hasql.Codecs.RequestingOid qualified as RequestingOid
import Hasql.Codecs.Vocab.QualifiedTypeName qualified as Vocab.QualifiedTypeName
import Hasql.Codecs.Vocab.TypeInfo qualified as Vocab.TypeInfo
import Hasql.Platform.Prelude
import PostgreSQL.Binary.Decoding qualified as Binary

-- |
-- Composable decoder of composite values (rows, records).
newtype Composite a
  = Composite (RequestingOid.RequestingOid (Binary.Composite a))
  deriving
    (Functor, Applicative)
    via (Compose RequestingOid.RequestingOid Binary.Composite)

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
                  (Vocab.QualifiedTypeName.QualifiedTypeName (Value.toSchema imp) (Value.toTypeName imp))
                  (\typeInfo decoder -> Binary.typedValueComposite (if dimensionality == 0 then Vocab.TypeInfo.toBaseOid typeInfo else Vocab.TypeInfo.toArrayOid typeInfo) decoder)
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
                  (Vocab.QualifiedTypeName.QualifiedTypeName (Value.toSchema imp) (Value.toTypeName imp))
                  (\typeInfo decoder -> Binary.typedNullableValueComposite (if dimensionality == 0 then Vocab.TypeInfo.toBaseOid typeInfo else Vocab.TypeInfo.toArrayOid typeInfo) decoder)
                  (Value.toDecoder imp)
              )
