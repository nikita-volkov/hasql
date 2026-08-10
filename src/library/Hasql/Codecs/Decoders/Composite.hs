module Hasql.Codecs.Decoders.Composite where

import Hasql.Codecs.Decoders.NullableOrNot qualified as NullableOrNot
import Hasql.Codecs.Decoders.Value qualified as Value
import Hasql.CodecsCore.QualifiedTypeName qualified as CodecsCore.QualifiedTypeName
import Hasql.CodecsCore.TypeInfo qualified as CodecsCore.TypeInfo
import Hasql.Platform.Prelude
import Hasql.ToBeResolved qualified as ToBeResolved
import PostgreSQL.Binary.Decoding qualified as Binary

-- |
-- Composable decoder of composite values (rows, records).
newtype Composite a
  = Composite (ToBeResolved.ToBeResolved CodecsCore.QualifiedTypeName.QualifiedTypeName CodecsCore.TypeInfo.TypeInfo (Binary.Composite a))
  deriving
    (Functor, Applicative)
    via (Compose (ToBeResolved.ToBeResolved CodecsCore.QualifiedTypeName.QualifiedTypeName CodecsCore.TypeInfo.TypeInfo) Binary.Composite)

toValueDecoder :: Composite a -> ToBeResolved.ToBeResolved CodecsCore.QualifiedTypeName.QualifiedTypeName CodecsCore.TypeInfo.TypeInfo (Binary.Value a)
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
              ( (\typeInfo decoder -> Binary.typedValueComposite (if dimensionality == 0 then CodecsCore.TypeInfo.toBaseOid typeInfo else CodecsCore.TypeInfo.toArrayOid typeInfo) decoder)
                  <$> ToBeResolved.lookup (CodecsCore.QualifiedTypeName.QualifiedTypeName (Value.toSchema imp) (Value.toTypeName imp))
                  <*> Value.toDecoder imp
              )
  NullableOrNot.Nullable imp ->
    let dimensionality = Value.toDimensionality imp
        staticOid = if dimensionality == 0 then Value.toBaseOid imp else Value.toArrayOid imp
     in case staticOid of
          Just oid ->
            Composite (fmap (Binary.typedNullableValueComposite oid) (Value.toDecoder imp))
          Nothing ->
            Composite
              ( (\typeInfo decoder -> Binary.typedNullableValueComposite (if dimensionality == 0 then CodecsCore.TypeInfo.toBaseOid typeInfo else CodecsCore.TypeInfo.toArrayOid typeInfo) decoder)
                  <$> ToBeResolved.lookup (CodecsCore.QualifiedTypeName.QualifiedTypeName (Value.toSchema imp) (Value.toTypeName imp))
                  <*> Value.toDecoder imp
              )
