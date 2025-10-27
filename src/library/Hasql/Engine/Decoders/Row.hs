module Hasql.Engine.Decoders.Row where

import Hasql.Codecs.Decoders
import Hasql.Codecs.Decoders.Value qualified as Value
import Hasql.Codecs.RequestingOid qualified as RequestingOid
import Hasql.Comms.RowDecoder qualified
import Hasql.Platform.Prelude
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
  = Row (RequestingOid.RequestingOid Hasql.Comms.RowDecoder.RowDecoder a)
  deriving newtype
    (Functor, Applicative)

toDecoder ::
  Row a ->
  RequestingOid.RequestingOid Hasql.Comms.RowDecoder.RowDecoder a
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
          (Hasql.Comms.RowDecoder.nullableColumn (Just oid) . Binary.valueParser)
          (Value.toDecoder valueDecoder)
      Nothing -> do
        RequestingOid.hoistLookingUp
          (Value.toSchema valueDecoder, Value.toTypeName valueDecoder)
          ( \lookupResult ->
              Hasql.Comms.RowDecoder.nullableColumn (Just (chooseLookedUpOid valueDecoder lookupResult)) . Binary.valueParser
          )
          (Value.toDecoder valueDecoder)
  NonNullable valueDecoder ->
    Row case Value.toOid valueDecoder of
      Just oid ->
        RequestingOid.hoist
          (Hasql.Comms.RowDecoder.nonNullableColumn (Just oid) . Binary.valueParser)
          (Value.toDecoder valueDecoder)
      Nothing -> do
        RequestingOid.hoistLookingUp
          (Value.toSchema valueDecoder, Value.toTypeName valueDecoder)
          (\lookupResult -> Hasql.Comms.RowDecoder.nonNullableColumn (Just (chooseLookedUpOid valueDecoder lookupResult)) . Binary.valueParser)
          (Value.toDecoder valueDecoder)
  where
    chooseLookedUpOid valueDecoder (elementOid, arrayOid) =
      if Value.toDimensionality valueDecoder > 0
        then arrayOid
        else elementOid
