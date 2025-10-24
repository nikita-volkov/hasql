module Core.Decoders.Row where

import Codecs.Decoders
import Codecs.Decoders.Value qualified as Value
import Codecs.RequestingOid qualified as RequestingOid
import Hipq.RowDecoder qualified
import Platform.LookingUp qualified as LookingUp
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
    Row $ case Value.toBaseOid valueDecoder of
      -- Array type marker - needs dynamic lookup for array OID
      Just 1 ->
        let schema = Value.toSchema valueDecoder
            typeName = Value.toTypeName valueDecoder
         in LookingUp.LookingUp [(schema, typeName)] $ \lookupOid ->
              let (baseOid, arrayOid) = lookupOid (schema, typeName)
                  binaryValueDecoder = RequestingOid.toBase (Value.toDecoder valueDecoder) (fromList [((schema, typeName), (baseOid, arrayOid))])
               in Hipq.RowDecoder.nullableColumn (Just arrayOid) (Binary.valueParser binaryValueDecoder)
      -- Statically known OID - use it directly
      Just oid ->
        RequestingOid.hoist
          (Hipq.RowDecoder.nullableColumn (Just oid) . Binary.valueParser)
          (Value.toDecoder valueDecoder)
      -- Dynamically looked-up OID (scalar type) - request it
      Nothing ->
        let schema = Value.toSchema valueDecoder
            typeName = Value.toTypeName valueDecoder
         in LookingUp.LookingUp [(schema, typeName)] $ \lookupOid ->
              let (baseOid, arrayOid) = lookupOid (schema, typeName)
                  binaryValueDecoder = RequestingOid.toBase (Value.toDecoder valueDecoder) (fromList [((schema, typeName), (baseOid, arrayOid))])
               in Hipq.RowDecoder.nullableColumn (Just baseOid) (Binary.valueParser binaryValueDecoder)
  NonNullable valueDecoder ->
    Row $ case Value.toBaseOid valueDecoder of
      -- Array type marker - needs dynamic lookup for array OID
      Just 1 ->
        let schema = Value.toSchema valueDecoder
            typeName = Value.toTypeName valueDecoder
         in LookingUp.LookingUp [(schema, typeName)] $ \lookupOid ->
              let (baseOid, arrayOid) = lookupOid (schema, typeName)
                  binaryValueDecoder = RequestingOid.toBase (Value.toDecoder valueDecoder) (fromList [((schema, typeName), (baseOid, arrayOid))])
               in Hipq.RowDecoder.nonNullableColumn (Just arrayOid) (Binary.valueParser binaryValueDecoder)
      -- Statically known OID - use it directly
      Just oid ->
        RequestingOid.hoist
          (Hipq.RowDecoder.nonNullableColumn (Just oid) . Binary.valueParser)
          (Value.toDecoder valueDecoder)
      -- Dynamically looked-up OID (scalar type) - request it
      Nothing ->
        let schema = Value.toSchema valueDecoder
            typeName = Value.toTypeName valueDecoder
         in LookingUp.LookingUp [(schema, typeName)] $ \lookupOid ->
              let (baseOid, arrayOid) = lookupOid (schema, typeName)
                  binaryValueDecoder = RequestingOid.toBase (Value.toDecoder valueDecoder) (fromList [((schema, typeName), (baseOid, arrayOid))])
               in Hipq.RowDecoder.nonNullableColumn (Just baseOid) (Binary.valueParser binaryValueDecoder)
