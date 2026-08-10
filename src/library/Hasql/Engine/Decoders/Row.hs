module Hasql.Engine.Decoders.Row where

import Hasql.Codecs.Decoders
import Hasql.Codecs.Decoders.Value qualified as Value
import Hasql.CodecsVocab.QualifiedTypeName qualified as CodecsVocab.QualifiedTypeName
import Hasql.CodecsVocab.TypeInfo qualified as CodecsVocab.TypeInfo
import Hasql.Comms.RowDecoder qualified
import Hasql.Platform.Prelude
import Hasql.ToBeResolved qualified as ToBeResolved
import PostgreSQL.Binary.Decoding qualified as Binary

-- |
-- Decoder of an individual row,
-- which gets composed of column value decoders.
-- E.g.:
--
-- @
-- x :: 'Row' (Maybe Int64, Text, TimeOfDay)
-- x = (,,) '<$>' ('column' . 'nullable') 'int8' '<*>' ('column' . 'nonNullable') 'text' '<*>' ('column' . 'nonNullable') 'time'
-- @
newtype Row a
  = Row (ToBeResolved.ToBeResolved CodecsVocab.QualifiedTypeName.QualifiedTypeName CodecsVocab.TypeInfo.TypeInfo (Hasql.Comms.RowDecoder.RowDecoder a))
  deriving
    (Functor, Applicative, Filterable)
    via (Compose (ToBeResolved.ToBeResolved CodecsVocab.QualifiedTypeName.QualifiedTypeName CodecsVocab.TypeInfo.TypeInfo) Hasql.Comms.RowDecoder.RowDecoder)

toDecoder ::
  Row a ->
  ToBeResolved.ToBeResolved CodecsVocab.QualifiedTypeName.QualifiedTypeName CodecsVocab.TypeInfo.TypeInfo (Hasql.Comms.RowDecoder.RowDecoder a)
toDecoder (Row f) = f

-- |
-- Lift an individual value decoder to a composable row decoder.
{-# INLINE column #-}
column :: NullableOrNot Value a -> Row a
column = \case
  Nullable valueDecoder ->
    Row case Value.toOid valueDecoder of
      Just oid ->
        fmap
          (Hasql.Comms.RowDecoder.nullableColumn (Just oid) . Binary.valueParser)
          (Value.toDecoder valueDecoder)
      Nothing ->
        ( \lookupResult decoder ->
            Hasql.Comms.RowDecoder.nullableColumn (Just (chooseLookedUpOid valueDecoder lookupResult)) (Binary.valueParser decoder)
        )
          <$> ToBeResolved.lookup (CodecsVocab.QualifiedTypeName.QualifiedTypeName (Value.toSchema valueDecoder) (Value.toTypeName valueDecoder))
          <*> Value.toDecoder valueDecoder
  NonNullable valueDecoder ->
    Row case Value.toOid valueDecoder of
      Just oid ->
        fmap
          (Hasql.Comms.RowDecoder.nonNullableColumn (Just oid) . Binary.valueParser)
          (Value.toDecoder valueDecoder)
      Nothing ->
        (\lookupResult decoder -> Hasql.Comms.RowDecoder.nonNullableColumn (Just (chooseLookedUpOid valueDecoder lookupResult)) (Binary.valueParser decoder))
          <$> ToBeResolved.lookup (CodecsVocab.QualifiedTypeName.QualifiedTypeName (Value.toSchema valueDecoder) (Value.toTypeName valueDecoder))
          <*> Value.toDecoder valueDecoder
  where
    chooseLookedUpOid valueDecoder typeInfo =
      if Value.toDimensionality valueDecoder > 0
        then CodecsVocab.TypeInfo.toArrayOid typeInfo
        else CodecsVocab.TypeInfo.toBaseOid typeInfo
