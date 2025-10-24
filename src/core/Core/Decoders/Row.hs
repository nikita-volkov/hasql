module Core.Decoders.Row where

import Codecs.Decoders
import Codecs.Decoders.Value qualified as Value
import Codecs.RequestingOid qualified as RequestingOid
import Hipq.RowDecoder qualified
import Platform.Prelude
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
  = Row (RequestingOid.RequestingOid Hipq.RowDecoder.RowDecoder a)
  deriving newtype
    (Functor, Applicative)

toDecoder ::
  Row a ->
  RequestingOid.RequestingOid Hipq.RowDecoder.RowDecoder a
toDecoder (Row f) = f

-- |
-- Lift an individual value decoder to a composable row decoder.
{-# INLINEABLE column #-}
column :: NullableOrNot Value a -> Row a
column = \case
  Nullable valueDecoder ->
    Row case Value.toOid valueDecoder of
      Just oid ->
        RequestingOid.hoist
          (Hipq.RowDecoder.nullableColumn (Just oid) . Binary.valueParser)
          (Value.toDecoder valueDecoder)
      Nothing -> do
        RequestingOid.hoistLookingUp
          (Value.toSchema valueDecoder, Value.toTypeName valueDecoder)
          (\(oid, _) -> Hipq.RowDecoder.nullableColumn (Just oid) . Binary.valueParser)
          (Value.toDecoder valueDecoder)
  NonNullable valueDecoder ->
    Row case Value.toOid valueDecoder of
      Just oid ->
        RequestingOid.hoist
          (Hipq.RowDecoder.nonNullableColumn (Just oid) . Binary.valueParser)
          (Value.toDecoder valueDecoder)
      Nothing -> do
        RequestingOid.hoistLookingUp
          (Value.toSchema valueDecoder, Value.toTypeName valueDecoder)
          (\(oid, _) -> Hipq.RowDecoder.nonNullableColumn (Just oid) . Binary.valueParser)
          (Value.toDecoder valueDecoder)
