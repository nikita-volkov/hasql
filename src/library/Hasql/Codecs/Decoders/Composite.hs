module Hasql.Codecs.Decoders.Composite where

import Hasql.Codecs.Decoders.NullableOrNot qualified as NullableOrNot
import Hasql.Codecs.Decoders.Value qualified as Value
import Hasql.CodecsVocab.QualifiedTypeName qualified as CodecsVocab.QualifiedTypeName
import Hasql.CodecsVocab.TypeInfo qualified as CodecsVocab.TypeInfo
import Hasql.Platform.Prelude
import Hasql.ToBeResolved qualified as ToBeResolved
import PostgreSQL.Binary.Decoding qualified as Binary

-- |
-- Composable decoder of composite values (rows, records).
newtype Composite a
  = Composite (ToBeResolved.ToBeResolved CodecsVocab.QualifiedTypeName.QualifiedTypeName CodecsVocab.TypeInfo.TypeInfo (Binary.Composite a))
  deriving
    (Functor, Applicative)
    via (Compose (ToBeResolved.ToBeResolved CodecsVocab.QualifiedTypeName.QualifiedTypeName CodecsVocab.TypeInfo.TypeInfo) Binary.Composite)

toValueDecoder :: Composite a -> ToBeResolved.ToBeResolved CodecsVocab.QualifiedTypeName.QualifiedTypeName CodecsVocab.TypeInfo.TypeInfo (Binary.Value a)
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
              ( (\typeInfo decoder -> Binary.typedValueComposite (if dimensionality == 0 then CodecsVocab.TypeInfo.toBaseOid typeInfo else CodecsVocab.TypeInfo.toArrayOid typeInfo) decoder)
                  <$> ToBeResolved.lookup (CodecsVocab.QualifiedTypeName.QualifiedTypeName (Value.toSchema imp) (Value.toTypeName imp))
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
              ( (\typeInfo decoder -> Binary.typedNullableValueComposite (if dimensionality == 0 then CodecsVocab.TypeInfo.toBaseOid typeInfo else CodecsVocab.TypeInfo.toArrayOid typeInfo) decoder)
                  <$> ToBeResolved.lookup (CodecsVocab.QualifiedTypeName.QualifiedTypeName (Value.toSchema imp) (Value.toTypeName imp))
                  <*> Value.toDecoder imp
              )
